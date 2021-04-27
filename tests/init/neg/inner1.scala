class Foo {
  new this.Inner

  val list = List(1, 2, 3) // error, as Inner access `this.list`

  val inner: Inner = new this.Inner // ok, `list` is instantiated
  lib.escape(inner) // error

  val name = "good"

  class Inner {
    val len = list.size
  }
}

object lib {
  def escape(x: Foo#Inner): Unit = ???
}
