package dotty.tools.sbtplugin

object CompatMode {
  final val BinaryCompatible = 0
  final val SourceAndBinaryCompatible = 1

  val value = {
    val VersionRE = """^\d+\.(\d+)\.(\d+)""".r
    Versions.developedVersion match {
      case VersionRE(_, "0")   => CompatMode.BinaryCompatible
      case _                   => CompatMode.SourceAndBinaryCompatible
    }
  }
}
