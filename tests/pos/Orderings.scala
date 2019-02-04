object Orderings {

  // A type class:
  trait Ord[T] { def less(x: T, y: T): Boolean }

  implicit val intOrd: Ord[Int] = new {
    def less(x: Int, y: Int) = x < y
  }

  implicit def listOrd[T](implicit ev: Ord[T]): Ord[List[T]] = new {
    def less(xs: List[T], ys: List[T]): Boolean =
      if ys.isEmpty then false
      else if xs.isEmpty then true
      else if xs.head == ys.head then less(xs.tail, ys.tail)
      else ev.less(xs.head, ys.head)
  }

  def isLess[T]: T => T => given Ord[T] => Boolean =
    x => y => implicitly[Ord[T]].less(x, y)
}
