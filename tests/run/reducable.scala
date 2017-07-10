object Test extends App {
  val xs = List(1, 2, 3)

  object Cons {
    var count = 0

    def unapply[T](xs: List[T]): Option[(T, List[T])] = {
      count += 1
      xs match {
        case x :: xs1 => Some((x, xs1))
        case _ => None
      }
    }
  }

  val res = xs match {
    case Cons(0, Nil) => 1
    case Cons(_, Nil) => 2
    case Cons(0, _)   => 3
    case Cons(1, ys)  => 4
  }

  assert(res == 4, res)
  assert(Cons.count ==1, Cons.count)
}
