class Foo(n: Partial[String]) {
  foo(new Foo("Jack"))         // recursive creation

  val name: String = n        // error
  name.length                 // error

  private def foo(o: Foo) = o.name
}