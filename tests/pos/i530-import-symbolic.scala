object P {
  def !#@ : Nothing = ???
}

object Test {
  import P.!#@
  def f = !#@
}
