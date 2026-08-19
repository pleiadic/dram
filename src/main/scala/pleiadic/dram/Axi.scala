package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

object AxiBurst {
  val fixed = 0.U(2.W)
  val increment = 1.U(2.W)
  val wrap = 2.U(2.W)
}

object AxiResponse {
  val okay = 0.U(2.W)
}

class AxiAddress(addressWidth: Int, idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val address = UInt(addressWidth.W)
  val length = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
}

class AxiWriteData(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val strobe = UInt((dataWidth / 8).W)
  val last = Bool()
}

class AxiWriteResponse(idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val response = UInt(2.W)
}

class AxiReadData(dataWidth: Int, idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val data = UInt(dataWidth.W)
  val response = UInt(2.W)
  val last = Bool()
}

class Axi4SlavePort(addressWidth: Int, dataWidth: Int, idWidth: Int) extends Bundle {
  val aw = Flipped(Decoupled(new AxiAddress(addressWidth, idWidth)))
  val w = Flipped(Decoupled(new AxiWriteData(dataWidth)))
  val b = Decoupled(new AxiWriteResponse(idWidth))
  val ar = Flipped(Decoupled(new AxiAddress(addressWidth, idWidth)))
  val r = Decoupled(new AxiReadData(dataWidth, idWidth))
}

private class AxiReadTag(idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val last = Bool()
}

/**
  * Equal-width AXI4 slave to LiteDRAM Native bridge.
  *
  * Address and data channels are buffered independently. Native write data is
  * released only after a matching write command has been accepted. Responses
  * are ordered, always return OKAY, and preserve the AXI transaction ID.
  */
class Axi4ToNative(
    config: DramConfig,
    idWidth: Int = 4,
    maxBurstLength: Int = 256,
    addressQueueDepth: Int = 4,
    writeDataQueueDepth: Int = 16,
    readOutstanding: Int = 16,
    baseAddress: BigInt = 0
) extends Module {
  require(idWidth >= 1)
  require(maxBurstLength >= 1 && maxBurstLength <= 256)
  require(addressQueueDepth >= 1 && writeDataQueueDepth >= 1 && readOutstanding >= 1)
  require(baseAddress >= 0 && baseAddress < (BigInt(1) << config.addressBits))
  require(baseAddress % (config.dataBits / 8) == 0)

  private val addressWidth = config.addressBits
  private val nativeAddressWidth = config.addressBits - config.byteOffsetBits
  private val countWidth = 9
  private val reservationWidth = log2Ceil(writeDataQueueDepth + 1).max(1)
  private val fullBeatSize = log2Ceil(config.dataBits / 8)

  val io = IO(new Bundle {
    val axi = new Axi4SlavePort(addressWidth, config.dataBits, idWidth)
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val awQueue = Module(new Queue(new AxiAddress(addressWidth, idWidth), addressQueueDepth))
  private val wQueue = Module(new Queue(new AxiWriteData(config.dataBits), writeDataQueueDepth))
  private val arQueue = Module(new Queue(new AxiAddress(addressWidth, idWidth), addressQueueDepth))
  private val readTags = Module(new Queue(new AxiReadTag(idWidth), readOutstanding))

  awQueue.io.enq <> io.axi.aw
  wQueue.io.enq <> io.axi.w
  arQueue.io.enq <> io.axi.ar

  when(io.axi.aw.fire) {
    assert(io.axi.aw.bits.length < maxBurstLength.U, "AXI write burst exceeds maxBurstLength")
    assert(io.axi.aw.bits.size <= fullBeatSize.U, "AXI write beat is wider than the Native port")
    assert(io.axi.aw.bits.burst =/= 3.U, "AXI reserved write burst encoding")
  }
  when(io.axi.ar.fire) {
    assert(io.axi.ar.bits.length < maxBurstLength.U, "AXI read burst exceeds maxBurstLength")
    assert(io.axi.ar.bits.size <= fullBeatSize.U, "AXI read beat is wider than the Native port")
    assert(io.axi.ar.bits.burst =/= 3.U, "AXI reserved read burst encoding")
  }

  private val writeActive = RegInit(false.B)
  private val writeAddress = Reg(UInt(addressWidth.W))
  private val writeId = Reg(UInt(idWidth.W))
  private val writeLength = Reg(UInt(8.W))
  private val writeSize = Reg(UInt(3.W))
  private val writeBurst = Reg(UInt(2.W))
  private val writeCommandsRemaining = RegInit(0.U(countWidth.W))
  private val writeDataRemaining = RegInit(0.U(countWidth.W))
  private val writeReservations = RegInit(0.U(reservationWidth.W))

  private val responseValid = RegInit(false.B)
  private val responseId = Reg(UInt(idWidth.W))
  io.axi.b.valid := responseValid
  io.axi.b.bits.id := responseId
  io.axi.b.bits.response := AxiResponse.okay
  when(io.axi.b.fire) { responseValid := false.B }

  awQueue.io.deq.ready := !writeActive && !responseValid
  when(awQueue.io.deq.fire) {
    assert(awQueue.io.deq.bits.address >= baseAddress.U, "AXI write address is below baseAddress")
    writeActive := true.B
    writeAddress := awQueue.io.deq.bits.address
    writeId := awQueue.io.deq.bits.id
    writeLength := awQueue.io.deq.bits.length
    writeSize := awQueue.io.deq.bits.size
    writeBurst := awQueue.io.deq.bits.burst
    writeCommandsRemaining := awQueue.io.deq.bits.length +& 1.U
    writeDataRemaining := awQueue.io.deq.bits.length +& 1.U
  }

  private val readActive = RegInit(false.B)
  private val readAddress = Reg(UInt(addressWidth.W))
  private val readId = Reg(UInt(idWidth.W))
  private val readLength = Reg(UInt(8.W))
  private val readSize = Reg(UInt(3.W))
  private val readBurst = Reg(UInt(2.W))
  private val readCommandsRemaining = RegInit(0.U(countWidth.W))

  arQueue.io.deq.ready := !readActive
  when(arQueue.io.deq.fire) {
    assert(arQueue.io.deq.bits.address >= baseAddress.U, "AXI read address is below baseAddress")
    readActive := true.B
    readAddress := arQueue.io.deq.bits.address
    readId := arQueue.io.deq.bits.id
    readLength := arQueue.io.deq.bits.length
    readSize := arQueue.io.deq.bits.size
    readBurst := arQueue.io.deq.bits.burst
    readCommandsRemaining := arQueue.io.deq.bits.length +& 1.U
  }

  private def nextBurstAddress(address: UInt, length: UInt, size: UInt, burst: UInt): UInt = {
    val beatBytes = (1.U(addressWidth.W) << size)(addressWidth - 1, 0)
    val incremented = address + beatBytes
    val beats = Wire(UInt(addressWidth.W))
    beats := length +& 1.U
    val wrapBytes = (beats << size)(addressWidth - 1, 0)
    val wrapMask = wrapBytes - 1.U
    Mux(burst === AxiBurst.fixed, address,
      Mux(burst === AxiBurst.wrap,
        (address & ~wrapMask) | (incremented & wrapMask), incremented))
  }

  private val unreservedWriteData = wQueue.io.count > writeReservations
  private val writeCommandRequest = writeActive &&
    writeCommandsRemaining =/= 0.U && unreservedWriteData
  private val readCommandRequest = readActive &&
    readCommandsRemaining =/= 0.U && readTags.io.enq.ready

  private val ownerIdle :: ownerWrite :: ownerRead :: Nil = Enum(3)
  private val commandOwner = RegInit(ownerIdle)
  private val preferWrite = RegInit(true.B)
  private val selectWrite = WireDefault(false.B)
  private val selectRead = WireDefault(false.B)

  switch(commandOwner) {
    is(ownerWrite) { selectWrite := true.B }
    is(ownerRead) { selectRead := true.B }
    is(ownerIdle) {
      when(writeCommandRequest && readCommandRequest) {
        selectWrite := preferWrite
        selectRead := !preferWrite
      }.elsewhen(writeCommandRequest) {
        selectWrite := true.B
      }.elsewhen(readCommandRequest) {
        selectRead := true.B
      }
    }
  }

  io.nativeCommand.valid := Mux(selectWrite, writeCommandRequest,
    Mux(selectRead, readCommandRequest, false.B))
  io.nativeCommand.bits.write := selectWrite
  private val selectedAddress = Mux(selectWrite, writeAddress, readAddress)
  io.nativeCommand.bits.address :=
    ((selectedAddress - baseAddress.U) >> config.byteOffsetBits)(nativeAddressWidth - 1, 0)

  private val writeCommandFire = io.nativeCommand.fire && selectWrite
  private val readCommandFire = io.nativeCommand.fire && selectRead

  when(writeCommandFire) {
    writeCommandsRemaining := writeCommandsRemaining - 1.U
    writeAddress := nextBurstAddress(writeAddress, writeLength, writeSize, writeBurst)
    when(writeCommandsRemaining === 1.U) {
      commandOwner := ownerIdle
      preferWrite := false.B
    }.otherwise {
      commandOwner := ownerWrite
    }
  }
  when(readCommandFire) {
    readCommandsRemaining := readCommandsRemaining - 1.U
    readAddress := nextBurstAddress(readAddress, readLength, readSize, readBurst)
    when(readCommandsRemaining === 1.U) {
      readActive := false.B
      commandOwner := ownerIdle
      preferWrite := true.B
    }.otherwise {
      commandOwner := ownerRead
    }
  }

  io.nativeWriteData.valid := writeReservations =/= 0.U && wQueue.io.deq.valid
  io.nativeWriteData.bits.data := wQueue.io.deq.bits.data
  io.nativeWriteData.bits.byteEnable := wQueue.io.deq.bits.strobe
  wQueue.io.deq.ready := writeReservations =/= 0.U && io.nativeWriteData.ready
  private val writeDataFire = io.nativeWriteData.fire

  switch(Cat(writeCommandFire, writeDataFire)) {
    is("b10".U) { writeReservations := writeReservations + 1.U }
    is("b01".U) { writeReservations := writeReservations - 1.U }
  }

  when(writeDataFire) {
    val expectedLast = writeDataRemaining === 1.U
    assert(wQueue.io.deq.bits.last === expectedLast, "AXI WLAST does not match AWLEN")
    writeDataRemaining := writeDataRemaining - 1.U
    when(expectedLast) {
      assert(writeCommandsRemaining === 0.U, "AXI write data completed before all Native commands")
      writeActive := false.B
      responseValid := true.B
      responseId := writeId
    }
  }

  readTags.io.enq.valid := readCommandFire
  readTags.io.enq.bits.id := readId
  readTags.io.enq.bits.last := readCommandsRemaining === 1.U

  io.axi.r.valid := io.nativeReadData.valid && readTags.io.deq.valid
  io.axi.r.bits.id := readTags.io.deq.bits.id
  io.axi.r.bits.data := io.nativeReadData.bits.data
  io.axi.r.bits.response := AxiResponse.okay
  io.axi.r.bits.last := readTags.io.deq.bits.last
  io.nativeReadData.ready := io.axi.r.ready && readTags.io.deq.valid
  readTags.io.deq.ready := io.axi.r.ready && io.nativeReadData.valid
}
