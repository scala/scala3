class Foo {
  val len  = list.size
  val list = List(4, 6)   // warn

  lazy val len2 = list2.size  // ok
  val list2 = List(4, 6)

  lazy val len3 = name.size
  val len4 = len3 + 4
  val name = "hello"   // warn
}
