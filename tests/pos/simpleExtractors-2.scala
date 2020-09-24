class Foo {
  def bar(x: Any): Unit = x match {
    case Some(Some(i: Int)) => println(i)
    case Some(s as Some(i)) => println(s)
    case s as Some(r as Some(i)) => println(s)
  }
}
