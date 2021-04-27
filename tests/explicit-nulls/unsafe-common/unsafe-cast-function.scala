class Test {
  def f(g: String => String): Unit = ???

  def g1(x: String): String = ???
  def g2(x: String | Null): String = ???
  def g3(x: String): String | Null = ???
  def g4(x: String | Null): String | Null = ???

  def test = {
    f(g1) // ok
    f(g2) // ok: (String | Null => String) <:< (String => String)
    f(g3) // error
    f(g4) // error
  }
}
