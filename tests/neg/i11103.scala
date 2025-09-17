@main def test: Unit = {
  class Foo
  class Bar

  trait UpBnd[+A]
  trait P extends UpBnd[Foo]

  def pmatch[A, T <: UpBnd[A]](s: T): A = s match {
    case p: P =>
      new Foo // error
  }
}
