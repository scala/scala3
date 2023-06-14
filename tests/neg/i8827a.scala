package pkg

trait Order[A]

object Order {
  implicit def orderList[A](implicit orderA: Order[A]): Order[List[A]] = ???
}

class Foo

object Implicits {
  implicit def orderFoo: Order[Foo] = ???
}

@main def main: Unit =
  summon[Order[List[Foo]]] // error
