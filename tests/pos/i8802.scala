trait Foo[A, B] {
  type Out
}

object Test {

  type Bar[A]

  def unit: Bar[Unit] = ???
  def product[A, B](fst: Bar[A], snd: Bar[B])(implicit foo: Foo[A, B]): Bar[foo.Out] = ???

  implicit def foo[A]: Foo[A, Unit] { type Out = A } = ???

  def check[A](bar: Bar[A])(a: A): Unit = {}

  check(product(unit, unit))(()) // error
}