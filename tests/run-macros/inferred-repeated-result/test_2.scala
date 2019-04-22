class C {
  def foo1(s: String*) = s
  def foo2(s: String*) = {
    val s1 = s
    s
  }

  def bar1(s: String*) = Option(s)
  def bar2(s: String*) = {
    val o = Option(s)
    o
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    Macros.go(new C)
  }
}
