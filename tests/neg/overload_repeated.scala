object Test {
  def bar1(x: Any) = 1
  def bar1(x: String*) = 2

  assert(bar1("") == 1) // error: ambiguous in Scala 2 and Scala 3
}
