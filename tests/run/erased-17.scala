//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    val f: (erased Int) => Int =
      (erased x: Int) => { println("lambda"); 42 }
    f(foo)
  }

  def foo = {
    println("foo")
    42
  }
}
