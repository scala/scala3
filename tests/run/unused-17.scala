object Test {

  def main(args: Array[String]): Unit = {
    val f: unused Int => Int =
      unused (x: Int) => { println("lambda"); 42 }
    f(foo)
  }

  def foo = {
    println("foo")
    42
  }
}
