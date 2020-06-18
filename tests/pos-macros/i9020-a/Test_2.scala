case class Foo(x: String)

object Bar {
  println(Show.deriveWithMacro[Foo].show(Foo("")))
}
