package dotty.tools.dotc.config

enum ScalaRelease(val majorVersion: Int, val minorVersion: Int) extends Ordered[ScalaRelease]:
  case Release3_0 extends ScalaRelease(3, 0)
  case Release3_1 extends ScalaRelease(3, 1)

  def show = s"$majorVersion.$minorVersion"

  def compare(that: ScalaRelease) =
    val ord = summon[Ordering[(Int, Int)]]
    ord.compare((majorVersion, minorVersion), (that.majorVersion, that.minorVersion))

object ScalaRelease:
  def latest = Release3_1

  def parse(name: String) = name match
    case "3.0" => Some(Release3_0)
    case "3.1" => Some(Release3_1)
    case _ => None
