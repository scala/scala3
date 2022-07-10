// scalac: -Werror
sealed trait Coverage
sealed abstract case class Range(min: Double, max: Double) extends Coverage
case object Empty extends Coverage

object Test:
  def mkCoverage(min: Double, max: Double): Coverage =
    if min < max then new Range(min, max) {} else Empty

  def meth(c1: Coverage, c2: Coverage): Coverage = (c1, c2) match
    case (Empty, _)                     => Empty
    case (_, Empty)                     => Empty // was: Unreachable case
    case (Range(a1, b1), Range(a2, b2)) => mkCoverage(a1 max a2, b1 min b2)
