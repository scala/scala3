import caps.cap

class It[A]

class Filter[A](val underlying: It[A]^, val p: A ->{cap, underlying} Boolean) extends It[A]
object Filter:
  def apply[A](underlying: It[A]^, p: A => Boolean): Filter[A]^{cap, p, underlying} =
    underlying match
      case filter: Filter[A]^ =>
        val x = new Filter(filter.underlying, a => filter.p(a) && p(a))
        x: Filter[A]^{filter, p}