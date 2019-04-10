object Test {

  def main(args: Array[String]): Unit = {
    new Bar(c)(c).foo()
    identity(new Bar(c)(c)).foo()
  }

  def c = {
    println("c")
    3
  }
}

class Bar(x: Int) erased (y: Int) extends AnyVal {
  def foo() = x
}
