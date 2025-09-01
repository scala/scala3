//> using options -experimental

@main def Test: Unit = {
  val foo = makeClass("Bar")
  foo.getSelf
}
