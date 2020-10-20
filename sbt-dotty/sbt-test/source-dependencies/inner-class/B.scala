object B {
  def main(args: Array[String]): Unit = {
    val innerClass = new A.InnerClass
    val x = innerClass.foo
    println(x)
  }
}
