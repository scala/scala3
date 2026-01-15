package dotty.tools.tasty

import scala.annotation.internal.sharable

case class TastyVersion private(major: Int, minor: Int, experimental: Int) {
  def isExperimental: Boolean = experimental > 0

  def nextStable: TastyVersion = copy(experimental = 0)

  def minStable: TastyVersion = copy(minor = 0, experimental = 0)

  def show: String = {
    val suffix = if (isExperimental) s"-experimental-$experimental" else ""
    s"$major.$minor$suffix"
  }

  def kind: String =
    if (isExperimental) "experimental TASTy" else "TASTy"

  def validRange: String = {
    val min = TastyVersion(major, 0, 0)
    val max = if (experimental == 0) this else TastyVersion(major, minor - 1, 0)
    val extra = Option(this).filter(_ => experimental > 0)
    s"stable TASTy from ${min.show} to ${max.show}${extra.fold("")(e => s", or exactly ${e.show}")}"
  }
}

object TastyVersion {

  @sharable
  private val cache: java.util.concurrent.ConcurrentHashMap[TastyVersion, TastyVersion] =
    new java.util.concurrent.ConcurrentHashMap()

  def apply(major: Int, minor: Int, experimental: Int): TastyVersion = {
    val version = new TastyVersion(major, minor, experimental)
    val cachedVersion = cache.putIfAbsent(version, version)
    if (cachedVersion == null) version else cachedVersion
  }
}
