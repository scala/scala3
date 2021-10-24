object Test {
  object Unapply {
    def unapply[T](x: Int)(using T): Option[T] = ???
  }
  trait Foo
  object Impl extends Foo

  implicit val x: Foo = Impl
  implicit val distractor: Int = 5

  5 match {
    case Unapply(Impl: Foo) => ???
  }
}
