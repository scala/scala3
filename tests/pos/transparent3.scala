object DepNats {
  sealed trait List[+T] {
    transparent def length: Int =
      if (this.isInstanceOf[Nil.type]) 0
      else 1 + this.asInstanceOf[Cons[T]].tail.length
  }
  case object Nil extends List[Nothing]
  case class Cons[+T](head: T, tail: List[T]) extends List[T]

  Nil.length: 0
  // FIXME: Incorrectly evaluates isInstanceOf to false on second unrolling:
//  Cons(true, Nil).length: 1
}
