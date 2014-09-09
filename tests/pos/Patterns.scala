object Patterns {/*
  ('1', "1") match {
    case (digit, str) => true
    case _ => false
  }

  object Breakdown {
    def unapplySeq(x: Int): Some[List[String]] = Some(List("", "there"))
  }

  object Test2 {
    42 match {
      case Breakdown("") =>  // needed to trigger bug
      case Breakdown("foo") =>  // needed to trigger bug
      case Breakdown("", who) => println ("hello " + who)
    }
  }

  val names = List("a", "b", "c")
  object SeqExtractors {
    val y = names match {
      case List(x, z) => x
      case List(x) => x
      case List() => ""
    }
    val yy: String = y
  }



  val xs = List('2' -> "ABC", '3' -> "DEF")

  xs filter {
    case (digit, str) => true
    case _ => false
  }

  (xs: Any) match {
    case x: Int @unchecked => true
    case xs: List[Int @ unchecked] => true
    case _ => false
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: xs1 => x + sum(xs1)
  }

  def len[T](xs: List[T]): Int = xs match {
    case _ :: xs1 => 1 + len(xs1)
    case Nil => 0
  }*/

  final def sameLength[T](xs: List[T], ys: List[T]): Boolean = xs match {
    case _ :: xs1 =>
      ys match {
        case _ :: ys1 => sameLength(xs1, ys1)
        case _ => false
      }
    case _ => ys.isEmpty
  }
}
