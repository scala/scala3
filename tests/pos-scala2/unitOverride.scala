trait C {

  def f: Unit
  def g(): Unit

}

object Test extends C {

  val x: Any = ???
  x.toString  // OK

  def f() = () // error: bad override
  def g = () // error: bad override

}
