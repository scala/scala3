@main def test: Unit = {
  class Foo
  class Bar

  trait UpBnd[+A]
  trait P extends UpBnd[Foo]

  def pmatch[A, T <: UpBnd[A]](s: T): A = s match {
    case p: P =>
      new Foo // error
  }

  class UpBndAndB extends UpBnd[Bar] with P
  // ClassCastException: Foo cannot be cast to Bar
  val x = pmatch(new UpBndAndB)
}
