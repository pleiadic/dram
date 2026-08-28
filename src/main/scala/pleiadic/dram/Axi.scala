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
  val exOkay = 1.U(2.W)
  val exclusiveOkay = exOkay
  val slaveError = 2.U(2.W)
  val decodeError = 3.U(2.W)
}

class AxiAddress(addressWidth: Int, idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val address = UInt(addressWidth.W)
  val length = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
  val lock = Bool()
  val cache = UInt(4.W)
  val prot = UInt(3.W)
  val qos = UInt(4.W)
  val region = UInt(4.W)
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

class Axi4ToNativeIo(config: DramConfig, idWidth: Int) extends Bundle {
  val axi = new Axi4SlavePort(config.addressBits, config.dataBits, idWidth)
  val nativeCommand = Decoupled(new NativeCommand(config))
  val nativeWriteData = Decoupled(new NativeWriteData(config))
  val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
}

private class AxiReadTag(idWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val last = Bool()
  val response = UInt(2.W)
}

/**
  * Equal-width AXI4 slave to LiteDRAM Native bridge.
  *
  * Address and data channels are buffered independently. Native write data is
  * released only after a matching write command has been accepted. Responses
  * are ordered, always return OKAY, and preserve the AXI transaction ID.
  */
private class Axi4ToNativeBridge(
    config: DramConfig,
    idWidth: Int = 4,
    maxBurstLength: Int = 256,
    addressQueueDepth: Int = 4,
    writeDataQueueDepth: Int = 16,
    readOutstanding: Int = 16,
    baseAddress: BigInt = 0
) extends Module {
  require(idWidth >= 1)
  require(idWidth <= 8, "exclusive monitor table supports at most 256 AXI IDs")
  require(maxBurstLength >= 1 && maxBurstLength <= 256)
  require(addressQueueDepth >= 1 && writeDataQueueDepth >= 1 && readOutstanding >= 1)
  require(baseAddress >= 0 && baseAddress < (BigInt(1) << config.addressBits))
  require(baseAddress % (config.dataBits / 8) == 0)

  private val addressWidth = config.addressBits
  private val nativeAddressWidth = config.addressBits - config.byteOffsetBits
  private val countWidth = 9
  private val reservationWidth = log2Ceil(writeDataQueueDepth + 1).max(1)
  private val fullBeatSize = log2Ceil(config.dataBits / 8)

  val io = IO(new Axi4ToNativeIo(config, idWidth))

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
  private val writeDrop = RegInit(false.B)
  private val writeResponse = RegInit(AxiResponse.okay)
  private val writeCommandsRemaining = RegInit(0.U(countWidth.W))
  private val writeDataRemaining = RegInit(0.U(countWidth.W))
  private val writeReservations = RegInit(0.U(reservationWidth.W))

  private val responseValid = RegInit(false.B)
  private val responseId = Reg(UInt(idWidth.W))
  private val responseCode = RegInit(AxiResponse.okay)
  io.axi.b.valid := responseValid
  io.axi.b.bits.id := responseId
  io.axi.b.bits.response := responseCode
  when(io.axi.b.fire) { responseValid := false.B }

  // The monitor table is indexed by AXI ID. A global write epoch provides a
  // conservative global-exclusive monitor: any completed write invalidates
  // every older reservation, while each ID retains its own transaction shape.
  private val monitorCount = 1 << idWidth
  private val monitorValid = RegInit(VecInit(Seq.fill(monitorCount)(false.B)))
  private val monitorAddress = Reg(Vec(monitorCount, UInt(addressWidth.W)))
  private val monitorLength = Reg(Vec(monitorCount, UInt(8.W)))
  private val monitorSize = Reg(Vec(monitorCount, UInt(3.W)))
  private val monitorBurst = Reg(Vec(monitorCount, UInt(2.W)))
  private val writeEpoch = RegInit(0.U(32.W))
  private val monitorEpoch = Reg(Vec(monitorCount, UInt(32.W)))

  awQueue.io.deq.ready := !writeActive && !responseValid
  when(awQueue.io.deq.fire) {
    assert(awQueue.io.deq.bits.address >= baseAddress.U, "AXI write address is below baseAddress")
    val monitorId = awQueue.io.deq.bits.id
    val monitorMatches = monitorValid(monitorId) &&
      monitorAddress(monitorId) === awQueue.io.deq.bits.address &&
      monitorLength(monitorId) === awQueue.io.deq.bits.length &&
      monitorSize(monitorId) === awQueue.io.deq.bits.size &&
      monitorBurst(monitorId) === awQueue.io.deq.bits.burst &&
      monitorEpoch(monitorId) === writeEpoch
    val exclusiveSuccess = awQueue.io.deq.bits.lock && monitorMatches
    writeActive := true.B
    writeAddress := awQueue.io.deq.bits.address
    writeId := awQueue.io.deq.bits.id
    writeLength := awQueue.io.deq.bits.length
    writeSize := awQueue.io.deq.bits.size
    writeBurst := awQueue.io.deq.bits.burst
    writeDrop := awQueue.io.deq.bits.lock && !monitorMatches
    writeResponse := Mux(exclusiveSuccess, AxiResponse.exclusiveOkay, AxiResponse.okay)
    writeCommandsRemaining := Mux(awQueue.io.deq.bits.lock && !monitorMatches,
      0.U, awQueue.io.deq.bits.length +& 1.U)
    writeDataRemaining := awQueue.io.deq.bits.length +& 1.U
    when(awQueue.io.deq.bits.lock) { monitorValid(monitorId) := false.B }
  }

  private val readActive = RegInit(false.B)
  private val readAddress = Reg(UInt(addressWidth.W))
  private val readId = Reg(UInt(idWidth.W))
  private val readLength = Reg(UInt(8.W))
  private val readSize = Reg(UInt(3.W))
  private val readBurst = Reg(UInt(2.W))
  private val readExclusive = RegInit(false.B)
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
    readExclusive := arQueue.io.deq.bits.lock
    readCommandsRemaining := arQueue.io.deq.bits.length +& 1.U
    when(arQueue.io.deq.bits.lock) {
      val monitorId = arQueue.io.deq.bits.id
      monitorValid(monitorId) := true.B
      monitorAddress(monitorId) := arQueue.io.deq.bits.address
      monitorLength(monitorId) := arQueue.io.deq.bits.length
      monitorSize(monitorId) := arQueue.io.deq.bits.size
      monitorBurst(monitorId) := arQueue.io.deq.bits.burst
      monitorEpoch(monitorId) := writeEpoch
    }
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

  io.nativeWriteData.valid := !writeDrop && writeReservations =/= 0.U && wQueue.io.deq.valid
  io.nativeWriteData.bits.data := wQueue.io.deq.bits.data
  io.nativeWriteData.bits.byteEnable := wQueue.io.deq.bits.strobe
  private val dropWriteData = writeActive && writeDrop && wQueue.io.deq.valid
  wQueue.io.deq.ready := Mux(writeDrop, writeActive,
    writeReservations =/= 0.U && io.nativeWriteData.ready)
  private val nativeWriteDataFire = io.nativeWriteData.fire
  private val writeDataFire = nativeWriteDataFire || dropWriteData

  switch(Cat(writeCommandFire, nativeWriteDataFire)) {
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
      responseCode := writeResponse
      when(!writeDrop) { writeEpoch := writeEpoch + 1.U }
    }
  }

  readTags.io.enq.valid := readCommandFire
  readTags.io.enq.bits.id := readId
  readTags.io.enq.bits.last := readCommandsRemaining === 1.U
  readTags.io.enq.bits.response :=
    Mux(readExclusive, AxiResponse.exclusiveOkay, AxiResponse.okay)

  io.axi.r.valid := io.nativeReadData.valid && readTags.io.deq.valid
  io.axi.r.bits.id := readTags.io.deq.bits.id
  io.axi.r.bits.data := io.nativeReadData.bits.data
  io.axi.r.bits.response := readTags.io.deq.bits.response
  io.axi.r.bits.last := readTags.io.deq.bits.last
  io.nativeReadData.ready := io.axi.r.ready && readTags.io.deq.valid
  readTags.io.deq.ready := io.axi.r.ready && io.nativeReadData.valid
}

/**
  * AXI4 slave to equal-width Native bridge. When `withReadModifyWrite` is set,
  * partial WSTRB writes are serialized into a Native read/full-mask write
  * sequence. This mode is suitable in front of ECC datapaths that cannot
  * update sub-ECC-word byte lanes directly.
  */
class Axi4ToNative(
    config: DramConfig,
    idWidth: Int = 4,
    maxBurstLength: Int = 256,
    addressQueueDepth: Int = 4,
    writeDataQueueDepth: Int = 16,
    readOutstanding: Int = 16,
    baseAddress: BigInt = 0,
    withReadModifyWrite: Boolean = false
) extends Module {
  val io = IO(new Axi4ToNativeIo(config, idWidth))

  private val bridge = Module(new Axi4ToNativeBridge(config, idWidth,
    maxBurstLength, addressQueueDepth, writeDataQueueDepth, readOutstanding,
    baseAddress))
  bridge.io.axi <> io.axi

  if (withReadModifyWrite) {
    val rmw = Module(new NativeReadModifyWrite(config))
    rmw.io.inputCommand <> bridge.io.nativeCommand
    rmw.io.inputWriteData <> bridge.io.nativeWriteData
    bridge.io.nativeReadData <> rmw.io.outputReadData
    io.nativeCommand <> rmw.io.outputCommand
    io.nativeWriteData <> rmw.io.outputWriteData
    rmw.io.inputReadData <> io.nativeReadData
  } else {
    io.nativeCommand <> bridge.io.nativeCommand
    io.nativeWriteData <> bridge.io.nativeWriteData
    bridge.io.nativeReadData <> io.nativeReadData
  }
}
