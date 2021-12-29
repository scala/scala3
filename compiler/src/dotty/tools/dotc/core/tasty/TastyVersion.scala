package dotty.tools.dotc.core.tasty

import dotty.tools.tasty.TastyFormat

case class TastyVersion(major: Int, minor: Int, experimental: Int) {
  def show = s"$major.$minor-$experimental"

  def isCompatibleWith(that: TastyVersion): Boolean = TastyFormat.isVersionCompatible(
    this.major, this.minor, this.experimental,
    that.major, that.minor, that.experimental
  )
}

object TastyVersion {
  def compilerVersion = TastyVersion(TastyFormat.MajorVersion, TastyFormat.MinorVersion, TastyFormat.ExperimentalVersion)

  def fromStableScalaRelease(majorVersion: Int, minorVersion: Int) = {
    val tastyMajor = majorVersion + 25
    val tastyMinor = minorVersion
    TastyVersion(tastyMajor, tastyMinor, 0)
  }

}