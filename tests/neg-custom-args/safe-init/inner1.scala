class Foo {
  val bar = new Bar(this)  // error
  new bar.Inner            // error

  new this.Inner           // error, as Inner access `this.list`

  val list = List(1, 2, 3)

  val inner: Inner = new this.Inner // ok, `list` is instantiated
  lib.escape(inner)                 // ok, `inner` is fully initialized

  val name = "good"

  class Inner {
    val len = list.size      // error: create new instance from line 5
  }
}

object lib {
  def escape(x: Foo#Inner): Unit = ???
}

class Bar(val foo: Raw[Foo]) {
  val inner = new foo.Inner   // error

  class Inner {
    val len = inner.len
  }
}