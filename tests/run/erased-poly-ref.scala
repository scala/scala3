object Test {

  def main(args: Array[String]): Unit = {
    fun(foo(bar(5))(bar(6)))
  }

  def fun erased (a: Int): Unit = println("fun")

  def foo[P] erased (x: Int) erased (y: Int): Int = 0

  def bar(x: Int) =  {
    println(x)
    x
  }
}
