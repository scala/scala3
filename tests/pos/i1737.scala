object Test {
  sealed trait Foo[A]
  case object FooI extends Foo[Int]
  case class FooS(b: Boolean) extends Foo[String]

  def algFoo[A](foo: Foo[A]): A =
    foo match {
      case FooI => 42
      case FooS(b) => "foo"
    }
}
