object Bar {
  class Foo {
    def m1(i: Int) = i + i1
    def m2(i: Int) = i + i2
  }
  def unapply(using f1: Foo)(i: Int): Option[Int] =
    if i == 0 then Some(f1.m1(i)) else Some(f1.m2(i))

  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = i1 match
    case Bar(i) => i
    case _ => 0
}

