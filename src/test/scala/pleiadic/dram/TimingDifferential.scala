package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.RawTester.test
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import scala.language.reflectiveCalls

/** Entry point used by scripts/run_python_differential.py. */
object TimingDifferential {
  private case class Vector(kind: String, parameter: Int,
      valids: IndexedSeq[Boolean], readys: IndexedSeq[Boolean])

  private def bits(value: String): IndexedSeq[Boolean] =
    value.toIndexedSeq.map {
      case '0' => false
      case '1' => true
      case other => throw new IllegalArgumentException(s"invalid oracle bit: $other")
    }

  private def load(path: String): Seq[Vector] =
    Files.readAllLines(Paths.get(path)).asScala
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .map { line =>
        val fields = line.split("\\t", -1)
        require(fields.length == 4, s"invalid oracle row: $line")
        val vector = Vector(fields(0), fields(1).toInt, bits(fields(2)), bits(fields(3)))
        require(vector.valids.length == vector.readys.length)
        vector
      }.toSeq

  private def check(vector: Vector): Unit = vector.kind match {
    case "TXXD" =>
      test(new TxxdController(vector.parameter)) { dut =>
        for (((valid, ready), cycle) <- vector.valids.zip(vector.readys).zipWithIndex) {
          dut.io.valid.poke(valid.B)
          dut.clock.step()
          dut.io.ready.expect(ready.B,
            s"tXXD=${vector.parameter} cycle=$cycle valid=$valid")
        }
      }
    case "TFAW" =>
      test(new TfawController(vector.parameter)) { dut =>
        for (((valid, ready), cycle) <- vector.valids.zip(vector.readys).zipWithIndex) {
          dut.io.valid.poke(valid.B)
          dut.clock.step()
          dut.io.ready.expect(ready.B,
            s"tFAW=${vector.parameter} cycle=$cycle valid=$valid")
        }
      }
    case other => throw new IllegalArgumentException(s"unknown oracle kind: $other")
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1, "usage: TimingDifferential <oracle.tsv>")
    val vectors = load(args(0))
    require(vectors.nonEmpty, "oracle contains no vectors")
    vectors.foreach(check)
    println(s"matched ${vectors.length} LiteDRAM timing vectors")
  }
}
