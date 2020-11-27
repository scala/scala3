class Foo {
  def bar(x: Any): Unit = x match {
    case Some(Some(i: Int)) => println(i)
    case Some(Some(i) as s) => println(s)
    case Some(Some(i) as r) as s => println(s)
  }
}
