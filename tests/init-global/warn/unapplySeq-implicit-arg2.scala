object Bar {
  class Foo
  def unapplySeq(using f1: Foo)(using f2: Foo)(seqi: Seq[Int])(using Foo): Option[Seq[Int]] =
    Some(i1 +: seqi)
  given Foo = new Foo
  val i1: Int = Seq(0) match {
    case Bar(i) => i
    case _ => 0
  }
}

