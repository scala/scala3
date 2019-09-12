trait Monad[T]
class Foo
object Foo {
  given as Monad[Foo]
}

opaque type Bar = Foo
object Bar {
  given as Monad[Bar] = summon[Monad[Foo]]
}

object Test {
  val mf = summon[Monad[Foo]]
  val mb = summon[Monad[Bar]]
}