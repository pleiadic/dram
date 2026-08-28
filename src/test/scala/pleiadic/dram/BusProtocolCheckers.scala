package pleiadic.dram

import chisel3._
import chiseltest._
import scala.collection.mutable
import scala.language.reflectiveCalls

/** Test-side AXI4 protocol observer, intentionally independent of DUT state. */
final class Axi4ProtocolChecker(dut: Axi4ToNative) {
  private case class Address(id: BigInt, address: BigInt, length: BigInt,
      size: BigInt, burst: BigInt, lock: Boolean, cache: BigInt, prot: BigInt,
      qos: BigInt, region: BigInt)
  private case class Write(data: BigInt, strobe: BigInt, last: Boolean)
  private case class WriteResponse(id: BigInt, response: BigInt)
  private case class Read(id: BigInt, data: BigInt, response: BigInt, last: Boolean)

  private var heldAw = Option.empty[Address]
  private var heldW = Option.empty[Write]
  private var heldAr = Option.empty[Address]
  private var heldB = Option.empty[WriteResponse]
  private var heldR = Option.empty[Read]
  private val writeAddresses = mutable.Queue.empty[(BigInt, Int)]
  private val writeBursts = mutable.Queue.empty[Int]
  private var currentWriteBeats = 0
  private val completedWrites = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
  private val reads = mutable.Map.empty[BigInt, mutable.Queue[Int]]

  private def address(channel: AxiAddress): Address = Address(
    channel.id.peek().litValue, channel.address.peek().litValue,
    channel.length.peek().litValue, channel.size.peek().litValue,
    channel.burst.peek().litValue, channel.lock.peek().litToBoolean,
    channel.cache.peek().litValue, channel.prot.peek().litValue,
    channel.qos.peek().litValue, channel.region.peek().litValue)

  private def checkHeld[T](name: String, previous: Option[T], valid: Boolean,
      ready: Boolean, payload: T): Option[T] = {
    previous.foreach(value => assert(valid && payload == value,
      s"AXI $name payload changed or VALID dropped while READY was low"))
    if (valid && !ready) Some(payload) else None
  }

  private def checkAddressRules(name: String, value: Address): Unit = {
    val beats = value.length.toInt + 1
    val bytes = 1 << value.size.toInt
    assert(value.burst != 3, s"AXI $name used reserved BURST encoding")
    if (value.burst != AxiBurst.fixed.litValue)
      assert((value.address & 0xfff) + beats * bytes <= 4096,
        s"AXI $name burst crossed a 4-KiB boundary")
    if (value.burst == AxiBurst.wrap.litValue) {
      assert(Set(2, 4, 8, 16).contains(beats),
        s"AXI $name WRAP burst has illegal length $beats")
      assert(value.address % bytes == 0,
        s"AXI $name WRAP address is not beat aligned")
    }
    if (value.lock)
      assert(beats <= 16 && beats * bytes <= 128,
        s"AXI $name exclusive burst exceeds architectural limits")
  }

  def sample(): Unit = {
    val awValid = dut.io.axi.aw.valid.peek().litToBoolean
    val awReady = dut.io.axi.aw.ready.peek().litToBoolean
    val aw = address(dut.io.axi.aw.bits)
    heldAw = checkHeld("AW", heldAw, awValid, awReady, aw)
    if (awValid && awReady) {
      checkAddressRules("AW", aw)
      writeAddresses.enqueue(aw.id -> (aw.length.toInt + 1))
    }

    val wValid = dut.io.axi.w.valid.peek().litToBoolean
    val wReady = dut.io.axi.w.ready.peek().litToBoolean
    val w = Write(dut.io.axi.w.bits.data.peek().litValue,
      dut.io.axi.w.bits.strobe.peek().litValue,
      dut.io.axi.w.bits.last.peek().litToBoolean)
    heldW = checkHeld("W", heldW, wValid, wReady, w)
    if (wValid && wReady) {
      currentWriteBeats += 1
      if (w.last) {
        writeBursts.enqueue(currentWriteBeats)
        currentWriteBeats = 0
      }
    }

    while (writeAddresses.nonEmpty && writeBursts.nonEmpty) {
      val (id, expectedBeats) = writeAddresses.dequeue()
      val observedBeats = writeBursts.dequeue()
      assert(observedBeats == expectedBeats,
        s"AXI WLAST ended after $observedBeats beats, AWLEN expected $expectedBeats")
      completedWrites(id) += 1
    }

    val bValid = dut.io.axi.b.valid.peek().litToBoolean
    val bReady = dut.io.axi.b.ready.peek().litToBoolean
    val b = WriteResponse(dut.io.axi.b.bits.id.peek().litValue,
      dut.io.axi.b.bits.response.peek().litValue)
    heldB = checkHeld("B", heldB, bValid, bReady, b)
    if (bValid && bReady) {
      assert(completedWrites(b.id) > 0,
        s"AXI B response for ID ${b.id} has no completed AW/W transaction")
      completedWrites(b.id) = completedWrites(b.id) - 1
    }

    val arValid = dut.io.axi.ar.valid.peek().litToBoolean
    val arReady = dut.io.axi.ar.ready.peek().litToBoolean
    val ar = address(dut.io.axi.ar.bits)
    heldAr = checkHeld("AR", heldAr, arValid, arReady, ar)
    if (arValid && arReady) {
      checkAddressRules("AR", ar)
      reads.getOrElseUpdate(ar.id, mutable.Queue.empty).enqueue(ar.length.toInt + 1)
    }

    val rValid = dut.io.axi.r.valid.peek().litToBoolean
    val rReady = dut.io.axi.r.ready.peek().litToBoolean
    val r = Read(dut.io.axi.r.bits.id.peek().litValue,
      dut.io.axi.r.bits.data.peek().litValue,
      dut.io.axi.r.bits.response.peek().litValue,
      dut.io.axi.r.bits.last.peek().litToBoolean)
    heldR = checkHeld("R", heldR, rValid, rReady, r)
    if (rValid && rReady) {
      val transactions = reads.getOrElse(r.id, mutable.Queue.empty)
      assert(transactions.nonEmpty,
        s"AXI R response for ID ${r.id} has no accepted AR transaction")
      val remaining = transactions.dequeue()
      assert(r.last == (remaining == 1),
        s"AXI RLAST mismatch for ID ${r.id}, $remaining beats remained")
      if (remaining > 1) transactions.prepend(remaining - 1)
    }
  }

  def finish(): Unit = {
    assert(heldAw.isEmpty && heldW.isEmpty && heldAr.isEmpty && heldB.isEmpty && heldR.isEmpty,
      "AXI simulation ended with a stalled channel")
    assert(writeAddresses.isEmpty && writeBursts.isEmpty && currentWriteBeats == 0,
      "AXI simulation ended with unmatched AW/W traffic")
    assert(completedWrites.values.forall(_ == 0),
      "AXI simulation ended with missing B responses")
    assert(reads.values.forall(_.isEmpty),
      "AXI simulation ended with incomplete R bursts")
  }
}

/** Test-side Avalon-MM protocol observer, independent of bridge internals. */
final class AvalonMmProtocolChecker(dut: AvalonMmToNative) {
  private case class Request(address: BigInt, read: Boolean, write: Boolean,
      burstCount: Int, data: BigInt, byteEnable: BigInt)
  private var held = Option.empty[Request]
  private var writeBeatsRemaining = 0
  private var readResponsesRemaining = 0

  def sample(): Unit = {
    val request = Request(dut.io.avalon.address.peek().litValue,
      dut.io.avalon.read.peek().litToBoolean,
      dut.io.avalon.write.peek().litToBoolean,
      dut.io.avalon.burstCount.peek().litValue.toInt,
      dut.io.avalon.writeData.peek().litValue,
      dut.io.avalon.byteEnable.peek().litValue)
    assert(!(request.read && request.write), "Avalon read and write asserted together")
    if (request.read || request.write)
      assert(request.burstCount >= 1, "Avalon burstCount must be non-zero")
    held.foreach(value => assert(request == value,
      "Avalon request changed while waitRequest was asserted"))
    held = if ((request.read || request.write) && dut.io.avalon.waitRequest.peek().litToBoolean)
      Some(request) else None

    val accepted = (request.read || request.write) &&
      !dut.io.avalon.waitRequest.peek().litToBoolean
    if (accepted && request.read) {
      assert(readResponsesRemaining == 0, "Avalon accepted overlapping read bursts")
      readResponsesRemaining = request.burstCount
    }
    if (accepted && request.write) {
      if (writeBeatsRemaining == 0) writeBeatsRemaining = request.burstCount
      writeBeatsRemaining -= 1
    }
    if (dut.io.avalon.readDataValid.peek().litToBoolean) {
      assert(readResponsesRemaining > 0, "Avalon returned read data without a request")
      readResponsesRemaining -= 1
    }
  }

  def finish(): Unit = {
    assert(held.isEmpty, "Avalon simulation ended with a stalled request")
    assert(writeBeatsRemaining == 0, "Avalon simulation ended mid-write-burst")
    assert(readResponsesRemaining == 0, "Avalon simulation ended with missing read data")
  }
}
