class Foo(n: Raw[String]) {
  foo(new Foo("Jack"))        // recursive creation

  val name: String = n
  name.length                 // error

  private def foo(o: Foo) = {
    def bar = o.name
    bar
  }
}
