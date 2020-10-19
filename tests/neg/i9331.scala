package object Foo {
  (1: Any) match {
    case Foo => 1  // error
  }
}
