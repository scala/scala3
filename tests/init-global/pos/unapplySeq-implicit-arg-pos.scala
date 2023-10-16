object Bar {
  class Foo {
    def m1(seq: Seq[Int]) = 0 +: seq
    def m2(seq: Seq[Int]) = i1 +: seq
  }
  def unapplySeq(using f1: Foo)(using f2: Foo)(seqi: Seq[Int])(using f3: Foo): Option[Seq[Int]] =
    if seqi(0) == 0 then Some(f1.m1(seqi)) else Some(f2.m2(seqi))

  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = Seq(i1) match
    case Bar(i) => i
    case _ => 0
}
