import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    val f: (Int @unused) => Int =
     (x: Int @unused) => { println("lambda"); 42 }
    f(foo)
  }

  def foo = {
    println("foo")
    42
  }
}
