@main def test: Unit = {
  class Foo
  class Bar extends Foo

  trait S[-A] {
    type T >: A
  }
  trait PFoo extends S[Foo]
  trait PBar extends S[Bar] {
    override type T = Bar
  }
  class PFooBar extends PBar with PFoo { // error
    override type T >: Bar  // error
  }

  def patmat[A](s: S[A]): s.T = s match {
    case p: (PFoo & s.type) => (new Foo): p.T
  }

  // ClassCastException: Foo cannot be cast to class Bar
  val x: Bar = patmat(new PFooBar: PBar)
}