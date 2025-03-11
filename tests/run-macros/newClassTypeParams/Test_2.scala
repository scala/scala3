//> using options -experimental

@main def Test: Unit = {
  val (cls, show) = makeClass("foo")
  println(cls.getClass)
  println(show)
}
