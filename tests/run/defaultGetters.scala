object Test {

  def foo[T](x: T = "abc") = x
  def bam(x: => Unit = ()) = x
  def bar(x: => Unit = { println("default bar"); () }) = x
  def baz(x: => String = { println("default baz"); "cde" }) = x

  def main(args: Array[String]): Unit = {
    assert(foo() == "abc")
    bam()
    bar()
    baz()
  }
}
