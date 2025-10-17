@main def Test: Unit = {
  val foo = makeClass("foo")
  println(foo.getClass)
  val bar = makeClass("bar")
  println(bar.getClass)
}
