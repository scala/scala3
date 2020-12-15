object Example extends App {
  final case class Foo[A](run: A ?=> Int)
}

object Example2 extends App {
  final case class Foo[A, B](run: (A, B) ?=> Int)
}
