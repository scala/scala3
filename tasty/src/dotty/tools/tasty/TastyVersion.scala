package dotty.tools.tasty

case class TastyVersion(major: Int, minor: Int, experimental: Int) {
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
    val extra = Option.when(experimental > 0)(this)
    s"stable TASTy from ${min.show} to ${max.show}${extra.fold("")(e => s", or exactly ${e.show}")}"
  }
}