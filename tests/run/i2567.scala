class TC

given tc : TC

class Foo with TC {
  println("hi")
}

object Test extends App {
  new Foo
  new Foo.with(tc)
  new Foo()
  new Foo().with(tc)
  Foo()
  Foo().with(tc)
}