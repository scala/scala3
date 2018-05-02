class Foo {
  val len  = list.size     // error
  val list = List(4, 6)

  lazy val len2 = list2.size  // ok
  val list2 = List(4, 6)
}