trait Monad[T]
class Foo
object Foo {
  given as Monad[Foo]
}

opaque type Bar = Foo
object Bar {
  given as Monad[Bar] = the[Monad[Foo]]
}

object Test {
  val mf = the[Monad[Foo]]
  val mb = the[Monad[Bar]]
}