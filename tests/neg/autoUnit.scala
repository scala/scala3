object Test {

  val x: Any = ???
  x.toString  // OK

  def f() = ()

  f // error: missing arguments
}
