object Test {
  trait Foo[Self]
  object Foo {
    class Type private(t: Any)

    object Type {
      def apply[T: Foo](t: T) = new Type(t)

      def unapply[T: Foo](t: Type): Option[T] = ???
    }
  }

  case class Instance1(a: String)
  given Foo[Instance1]()

  case class Instance2(b: Int)
  given Foo[Instance2]()

  case class NotAnInstance(b: Double)

  Foo.Type(Instance1("5")) match {
    case Foo.Type(Instance1(_: String)) => ???
    case Foo.Type(NotAnInstance(_)) => ??? // error: no implicit argument of type Test.Foo[T] was found
  }
}
