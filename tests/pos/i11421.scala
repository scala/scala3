class Foo {
  def ::(hd: String): Foo = ???
  def ::(hd: Boolean): Foo  = ???

  List(1, 2) match {
    case x :: tail => ()
    case _ => ()
  }
}