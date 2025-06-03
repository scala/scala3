class Foo {
  val bar = new Bar
  var x: bar.Inner = new bar.Inner

  class Inner {
    val len = list.size
  }

  class Bar {
    class Inner {
      val x  = g
      val len: Int = x.len
    }

    def g = new Foo.this.Inner
  }

  val list = List(1, 2, 3)  // warn
}
