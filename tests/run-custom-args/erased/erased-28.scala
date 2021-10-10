object Test {
  var a = true
  def main(args: Array[String]): Unit = {
    (if (a) erased (x: Int) => foo(x) else erased (x: Int) => bar(x))(x)
    a = false
    (if (a) erased (x: Int) => foo(x) else erased (x: Int) => bar(x))(x)
  }
  def foo(erased a: Int): Unit = {
    println("foo")
  }
  def bar(erased a: Int): Unit = {
    println("bar")
  }
  def x: Int = {
    println("x")
    42
  }
}
