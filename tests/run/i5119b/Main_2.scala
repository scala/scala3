
object Test {
  import Macro._

  def main(args: Array[String]): Unit = {
    println(ff(arg1 = foo(1), arg2 = foo(2)))
    println(ff(arg2 = foo(3), arg1 = foo(4)))
  }

  def foo(x: Any): Any = ()
}
