package pkg

trait Order[A]

object Order {
  given [A](using orderA: Order[A]): Order[Option[A]] = ???
}

class Foo

object Givens {
  given orderFoo: Order[Foo] = ???
}

@main def main: Unit =
  summon[Order[Option[Foo]]] // error
