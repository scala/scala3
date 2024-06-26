class Foo {
  val bar = new Bar
  var x: bar.Inner = new bar.Inner

  class Bar {
    class Inner {
      val x = g
      val len = x.size
    }

    def g = list
  }

  val list = List(1, 2, 3)    // warn
}
