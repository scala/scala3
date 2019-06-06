object Test {
  sealed trait Foo[A, B]
  final case class Bar[X](x: X) extends Foo[X, X]

  def foo[A, B](value: Foo[A, B], a: A => Int): B = value match {
    case Bar(x) => a(x); x
  }

  def bar[A, B](value: Foo[A, B], a: A => Int): B = value match {
    case b: Bar[a] => b.x
  }

  def err1[A, B](value: Foo[A, B], a: A => Int): B = value match {
    case b: Bar[A] => // spurious // error
      b.x
  }

  def err2[A, B](value: Foo[A, B], a: A => Int): B = value match {
    case b: Bar[B] => // spurious // error
      b.x
  }

  def fail[A, B](value: Foo[A, B], a: A => Int): B = value match {
    case b: Bar[Int] => // error
      b.x
  }
}
