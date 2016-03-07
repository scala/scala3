// shouldn't result in an abstract method error when run
abstract class NonJVMSam {
  def foo(x: Int): Int
}

object Test {
  def main(args: Array[String]): Unit = {
    val f: NonJVMSam = x => x + 1
    println(f.foo(3))
  }
}
