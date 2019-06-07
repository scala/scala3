object Test {
  var a = true
  def main(args: Array[String]): Unit = {
    (if (a) foo else bar)(x)
    a = false
    (if (a) foo else bar)(x)
  }
  def foo erased (a: Int): Unit = {
    println("foo")
  }
  def bar erased (a: Int): Unit = {
    println("bar")
  }
  def x: Int = {
    println("x")
    42
  }
}
