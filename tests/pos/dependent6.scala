object Foo {
  sealed trait List
  dependent case object Nil extends List
  dependent case class Cons(head: Int, tail: List) extends List


  dependent def cons1 =
    Cons(1, Cons(2, Nil)) match {
      case Cons(x, _) => x
    }

  dependent def cons2 = {
    val list1 = Cons(1, Cons(2, Nil))
    list1 match {
      case Cons(x, _) => x
    }
  }

  dependent def cons3 = {
    val list1 = Cons(1, Cons(2, Nil))
    list1 match {
      case Nil        => 0
      case Cons(x, _) => x
    }
  }

  dependent def cons4 =
    Cons(1, Cons(2, Nil)) match {
      case Cons(x, Cons(y, _)) => y
    }

  dependent def cons5 =
    Cons(1, Cons(2, Nil)) match {
      case Cons(x, Cons(y, zs)) => zs
    }

  cons1: 1
  cons2: 1
  // cons3: 1  // doesn't work yet (need to reduce `Match`es)
  cons4: 2
  cons5: {Nil}


  case class Some(x: Some)

  dependent def bla =
    (??? : Some) match {
      case Some(Some(x)) => x
    }
}