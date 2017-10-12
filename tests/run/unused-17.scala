import dotty.unused2

object Test {

  def main(args: Array[String]): Unit = {
    val f: (Int @unused2) => Int =
      (x: Int @unused2) => { println("lambda"); 42 }
    f(foo)
  }

  def foo = {
    println("foo")
    42
  }
}
