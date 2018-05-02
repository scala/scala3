class Foo {
  val len  = list.size     // error
  val list = List(4, 6)

  lazy val len2 = list2.size  // ok
  val list2 = List(4, 6)

  lazy val len3 = name.size   // error: trigger from len4
  val len4 = len3 + 4         // error
  val name = "hello"
}
