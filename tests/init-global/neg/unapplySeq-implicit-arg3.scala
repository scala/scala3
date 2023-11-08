object Bar {
  class Foo {
    def m(seq: Seq[Int]) = i1 +: seq 
  }
  def unapplySeq(using f1: Foo)(seqi: Seq[Int])(using Foo): Option[Seq[Int]] =
    Some(f1.m(seqi))
  given Foo = new Foo
  val i1: Int = Seq(0) match {
    case Bar(i, _) => i
    case _ => 0
  }
}

// nopos-error: No warnings can be incurred under -Werror.