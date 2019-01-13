object Test {
  def a = Symbol("a")
  def b = Symbol("B")
  def c = Symbol("+")
  //def d = '`\n` //error: unclosed character literal
  def e = Symbol("\u0041")
}
