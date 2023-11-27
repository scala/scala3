sealed trait Root[T]
case object C1 extends Root[Int]
case object C2 extends Root[String]
case class C3[X, Y]() extends Root[(X => X)|(Y => Y)|(X => Y)]
case class C4[X, Y]() extends Root[(X => X)|(Y => Y)|(X => Y)]

object TestGADT {

  def f[A <: Seq[?], B, Foo >: A => B](v: Root[Foo], u: Root[Foo]) = (v, u) match {
    case (C3(), C3()) =>
  }
  // The following line no longer type checks
  // f(C3[Seq[?], Long](), C4[Seq[?], Long]())
}
