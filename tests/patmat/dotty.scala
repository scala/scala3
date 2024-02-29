object IntEqualityTestTreeMaker {
  def unapply(xs: Int): Option[Int] = ???
}

class Test {
  def isBelow(n: Int, s: String): Boolean = false

  def foo(xs: List[(Int, String)]): Unit = xs.filter(isBelow.tupled) match {
    case Nil =>
    case matches =>
  }

  def linkCompanions(xs: List[(Int, Int)]): Unit = {
    xs.groupBy(_._1).foreach {
      case (_, List(x1, x2)) =>
      case _ => ()
    }
  }

  def bar(xs: List[(Int, String)]): Unit = xs match {
    case (x, s) :: Nil =>
    case Nil =>
    case _ =>
  }

  def patmat(alts: List[List[Int]]): Unit = alts.forall {
    case List(IntEqualityTestTreeMaker(_)) => false
    case _ => true
  }
}