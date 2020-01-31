trait TC[F[_]]

object TC {
  def check[F[_], A](x: F[A])(implicit F: TC[F]): Unit = ()
}

case class Foo[+E, +A](value: A)

object Foo {
  type WithList[+E, +A] = Foo[List[E], A]

  implicit def instance[E]: TC[[x] =>> Foo[E, x]] =
    new TC[[x] =>> Foo[E, x]] {}
}

val x1: Foo[List[String], Int] = Foo(1)
val x2: Foo.WithList[String, Int] = Foo(1)

def test =
  TC.check(x1)
  TC.check(x2)