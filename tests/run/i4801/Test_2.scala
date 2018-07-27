object Test {
  def main(args: Array[String]): Unit = {
    println(Macro.foo(true))
    println(Macro.foo(false))
    val x: Boolean = true
    println(Macro.foo(x))
  }
}
