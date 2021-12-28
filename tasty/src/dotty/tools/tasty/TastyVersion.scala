package dotty.tools.tasty

case class TastyVersion(major: Int, minor: Int, experimental: Int) {
  def show = "" + major + "." + minor + "-" + experimental
}

object TastyVersion {
  def compilerVersion = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, TastyFormat.ExperimentalVersion)

  def fromStableScalaRelease(majorVersion: Int, minorVersion: Int) = {
    val tastyMajor = majorVersion + 25
    val tastyMinor = minorVersion
    TastyVersion(tastyMajor, tastyMinor, 0)
  }

}
