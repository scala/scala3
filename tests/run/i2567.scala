class TC

given tc as TC

class Foo(using TC) {
  println("hi")
}

object Test extends App {
  new Foo
  new Foo(using tc)
  new Foo()
  new Foo()(using tc)
  Foo()
  Foo()(using tc)
}