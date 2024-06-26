class Foo {
  class Inner {
    val len = list.size
  }

  class Bar {
    val inner = new Inner
  }

  val bar: Bar = new Bar
  val list = List(1, 2, 3)       // warn
}
