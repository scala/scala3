class Foo {
  val len  = list.size
  val list = List(4, 6)    // warn

  lazy val len2 = list2.size  // ok
  val list2 = List(4, 6)
}