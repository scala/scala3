trait Foo:
  case class Bar[A](value: A)

object FooBar extends Foo

object Test:
  type UnwrapTypes[Xs] =
    Xs match
      case EmptyTuple => Xs
      case x *: xs => UnwrapTypes[x] *: UnwrapTypes[xs]
      case Foo#Bar[x] => UnwrapTypes[x]
      case String => String
      case Int => Int

  val x: Int = ??? : UnwrapTypes[FooBar.Bar[Int]]
  val tup: (Int, String)  = ??? : UnwrapTypes[(FooBar.Bar[Int], FooBar.Bar[String])]
end Test
