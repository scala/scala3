class Foo {
  val Some(_) = ???
  val (_, _, _) = ???
  ??? match {
    case Some(_) => ()
    case (_, _, _) => ()
  }
}
