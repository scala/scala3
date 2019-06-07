class TC
implied tc for TC

class Foo given TC {
  println("hi")
}

object Test extends App {
  new Foo
  new Foo given tc
  new Foo()
  new Foo() given tc
  Foo()
  Foo() given tc
}