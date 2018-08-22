object Test extends App {
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

  object Guard {
    var count = 0

    def apply(): Boolean = {
      count += 1
      false
    }
  }

  def reset(): Unit = {
    Cons.count = 0
    Guard.count = 0
  }

  val xs = List(1, 2, 3)
  test1(xs)
  reset()
  test2(xs)

  def test1(xs: List[Int]): Unit = {
    val res = xs match {
      case Cons(0, Nil) => 1
      case Cons(_, Nil) => 2
      case Cons(0, _)   => 3
      case Cons(1, ys)  => 4
    }

    assert(res == 4, res)
    assert(Cons.count == 1, Cons.count)
  }

  // #1313
  def test2(xs: List[Int]): Unit = {
    val res = xs match {
      case Cons(0, Nil) if Guard() => 1
      case Cons(0, Nil)            => 2
      case Cons(_, Nil) if Guard() => 3
      case Cons(_, Nil)            => 4
      case Cons(0, _) if Guard()   => 5
      case Cons(0, _)              => 6
      case Cons(1, ys) if Guard()  => 7
      case Cons(1, ys)             => 8
    }

    assert(res == 8, res)
    assert(Cons.count == 1, Cons.count)
    assert(Guard.count == 1, Guard.count)
  }
}
