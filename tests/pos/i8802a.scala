trait Foo[A1, B1] {
  type Out
}

object Test {

  type Bar[A2]

  def unit: Bar[Unit] = ???
  def product[A3, B3](fst: Bar[A3], snd: Bar[B3])(implicit foo: Foo[A3, B3]): Bar[foo.Out] = ???

  implicit def foo[A4]: Foo[A4, Unit] { type Out = A4 } = ???

  def check[A5](bar: Bar[A5])(a: A5): Unit = {}

  check(product(unit, unit)) // ok
  check(product(unit, unit)(summon[Foo[Unit, Unit]]))(()) // error
}