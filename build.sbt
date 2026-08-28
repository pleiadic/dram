ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "pleiadic"

val chiselVersion = "6.2.0"

libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % chiselVersion,
  "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.17" % Test
)

addCompilerPlugin(
  "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
)

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

// ChiselTest's in-process JNA backends load generated simulators by top name.
// Running suites concurrently can therefore bind a test to another suite's
// shared library on macOS. Keep the full hardware regression deterministic.
Test / parallelExecution := false
Test / fork := true
