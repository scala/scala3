object Test {
  def foo: Unit = {
    def a = () => ""
    bar({???; a}.apply())
  }
  def bar(a: => Any): Unit = {
    baz(a)
  }
  def baz(a: => Any): Unit = ()
  def main(args: Array[String]): Unit = {
    foo
  }
}
