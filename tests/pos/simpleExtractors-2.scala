class Foo {
  def bar(x: Any): Unit = x match {
    case Some(Some(i: Int)) => println(i)
    case Some(s @ Some(i)) => println(s)
    case s @ Some(r @ Some(i)) => println(s)
  }
}
