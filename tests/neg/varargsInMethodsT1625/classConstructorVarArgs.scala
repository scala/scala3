class Abc(val x: String*, val c: String*) { // error: varargs parameter must come last
  def test = ???
}