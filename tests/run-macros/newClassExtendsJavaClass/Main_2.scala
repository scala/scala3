//> using options -experimental

@main def Test: Unit = {
  val cls = makeClass("foo")
  println(cls.getClass)
  println(cls.getT())
}
