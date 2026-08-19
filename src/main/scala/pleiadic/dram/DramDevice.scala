package pleiadic.dram

case class DramDeviceTiming(variants: Map[String, SpdTiming]) {
  require(variants.nonEmpty)

  def select(variant: String = "default"): SpdTiming =
    variants.getOrElse(variant, variants.getOrElse("default",
      throw new IllegalArgumentException(s"timing variant '$variant' is not available")))
}

case class DramDevice(
  name: String,
  memType: String,
  registered: Boolean,
  geometry: DramSpdGeometry,
  technologyTimings: Map[String, DramDeviceTiming],
  speedgradeTimings: Map[String, Map[String, DramDeviceTiming]]) {

  def speedgrades: Seq[String] = speedgradeTimings.keys.filterNot(_ == "default").toSeq.sorted

  def timing(name: String, speedgrade: String = "default",
      fineRefreshMode: String = "1x"): Option[SpdTiming] = {
    val source = if (technologyTimings.contains(name)) technologyTimings
      else speedgradeTimings.getOrElse(speedgrade,
        throw new IllegalArgumentException(
          s"device $name has no speedgrade '$speedgrade'"))
    source.get(name).map(_.select(fineRefreshMode))
  }

  def toDramTiming(controllerClockHz: Double, nPhases: Int,
      speedgrade: String = "default", fineRefreshMode: String = "1x",
      readToPrecharge: SpdTiming = SpdTiming(cycles = 4)): DramTiming = {
    val selectedSpeedgrade = speedgradeTimings.getOrElse(speedgrade,
      throw new IllegalArgumentException(s"device $name has no speedgrade '$speedgrade'"))
    val technology = technologyTimings.view.mapValues(_.select(fineRefreshMode)).toMap
    val speed = selectedSpeedgrade.view.mapValues(_.select(fineRefreshMode)).toMap
    val numericSpeedgrade = speedgrade.toIntOption.getOrElse(0)
    DramSpd.toDramTiming(DramSpdData(memType, geometry, numericSpeedgrade,
      technology, speed), controllerClockHz, nPhases, readToPrecharge)
  }
}

/** Generated from the pinned LiteDRAM modules.py by scripts/generate_device_catalog.py. */
object DramDeviceCatalog {
  val devices: Seq[DramDevice] = DramDeviceCatalogGenerated.devices
  val byName: Map[String, DramDevice] = devices.map(device => device.name -> device).toMap

  def apply(name: String): DramDevice = byName.getOrElse(name,
    throw new IllegalArgumentException(s"unknown DRAM device: $name"))
}
