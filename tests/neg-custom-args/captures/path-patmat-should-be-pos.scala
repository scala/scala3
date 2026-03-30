import caps.any

class It[A]

class Filter[A](val underlying: It[A]^, val p: A ->{any, underlying} Boolean) extends It[A]
object Filter:
  def apply[A](underlying: It[A]^, p: A => Boolean): Filter[A]^{underlying, p} =
    underlying match
      case filter: Filter[A]^ =>
        val x = new Filter(filter.underlying, a => filter.p(a) && p(a))
        x: Filter[A]^{underlying, p} // error
          // !!! should work, it seems to be the case that the system does not recognize that
          // underlying and filter are aliases.

    // On the other hand, the following works:
    locally:
      val filter: underlying.type & Filter[A] = ???
      val a: It[A]^{filter.underlying} = ???
      val b: It[A]^{underlying} = a
      val x = new Filter(filter.underlying, a => filter.p(a) && p(a))
      x: Filter[A]^{underlying, p}

    locally:
      val filter: underlying.type & Filter[A]^ = ???
      val a: It[A]^{filter.underlying} = ???
      val b: It[A]^{underlying} = a
      val x = new Filter(filter.underlying, a => filter.p(a) && p(a))
      x: Filter[A]^{underlying, p}
