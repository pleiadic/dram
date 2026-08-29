package pleiadic.dram

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable

/** Backend-independent functional coverage bins for randomized Chisel tests.
  *
  * A test must explicitly close every bin with [[requireComplete]].  The JSON
  * artifact is diagnostic evidence; the assertion is the actual regression
  * gate, so coverage remains enforced with any chiseltest simulator backend.
  */
final class FunctionalCoverageBins(val scope: String, required: Seq[String]) {
  require(required.nonEmpty, s"$scope must define at least one coverage bin")
  require(required.distinct.size == required.size,
    s"$scope contains duplicate coverage bins")

  private val hits = mutable.LinkedHashMap.from(required.map(_ -> 0L))

  def hit(bin: String): Unit = {
    require(hits.contains(bin), s"$scope observed undeclared coverage bin '$bin'")
    hits(bin) += 1L
  }

  def hitWhen(bin: String, condition: Boolean): Unit = if (condition) hit(bin)

  def count(bin: String): Long = hits.getOrElse(bin,
    throw new IllegalArgumentException(s"$scope has no coverage bin '$bin'"))

  def requireComplete(): Unit = {
    val missing = hits.collect { case (bin, 0L) => bin }.toSeq
    assert(missing.isEmpty,
      s"$scope missed ${missing.size}/${hits.size} functional coverage bins: " +
        missing.mkString(", "))
    writeArtifact()
  }

  private def jsonString(value: String): String = {
    val escaped = value.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case character => character.toString
    }
    s"\"$escaped\""
  }

  private def writeArtifact(): Unit = {
    val directory = Paths.get("target", "functional-coverage-events")
    Files.createDirectories(directory)
    val fileName = scope.map {
      case character if character.isLetterOrDigit => character
      case _ => '_'
    } + ".json"
    val entries = hits.map { case (bin, value) =>
      s"    ${jsonString(bin)}: $value"
    }.mkString(",\n")
    val contents =
      s"""{
         |  "scope": ${jsonString(scope)},
         |  "covered": ${hits.size},
         |  "total": ${hits.size},
         |  "hits": {
         |$entries
         |  }
         |}
         |""".stripMargin
    Files.write(directory.resolve(fileName), contents.getBytes(StandardCharsets.UTF_8))
  }
}
