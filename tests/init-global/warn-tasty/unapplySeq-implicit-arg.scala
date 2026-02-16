object Bar {
  class Foo {
    def m1(seq: Seq[Int]) = 1 +: seq
    def m2(seq: Seq[Int]) = 2 +: seq
  }
  def unapplySeq(using f1: Foo)(seqi: Seq[Int]): Option[Seq[Int]] =
    if seqi(0) == 0 then Some(f1.m1(seqi)) else Some(f1.m2(seqi))

  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = Seq(i2) match // warn
    case Bar(i) => i
    case _ => 0
}
