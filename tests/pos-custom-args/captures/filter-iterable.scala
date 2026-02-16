import caps.any

class It[A]

class Filter[A](val underlying: It[A]^, val p: A ->{any, underlying} Boolean) extends It[A]
object Filter:
  def apply[A](underlying: It[A]^, p: A => Boolean): Filter[A]^{any, p, underlying} =
    underlying match
      case filter: Filter[A]^ =>
        val x = new Filter(filter.underlying, a => filter.p(a) && p(a))
        x: Filter[A]^{filter, p}