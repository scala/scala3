class Foo {
  new this.Inner

  val list = List(1, 2, 3) // warn, as Inner access `this.list`

  val inner: Inner = new this.Inner // ok, `list` is instantiated
  lib.escape(inner) // ok, can promote inner early

  val name = "good"

  class Inner {
    val len = list.size
  }
}

object lib {
  def escape(x: Foo#Inner): Unit = ???
}
