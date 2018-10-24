class Foo(val foo1: Raw[Foo], val foo2: Foo) {
  class Inner {
    val len = name.size    // error
  }

  new this.Inner  // error
  new foo1.Inner  // error
  new foo2.Inner

  val name = "hello"
}
