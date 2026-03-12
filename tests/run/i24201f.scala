// scalajs: --skip
abstract class Foo[T](value: => T, arg1: Int = 1, arg2: Int = 2):
  def getValue: T = value
  def getArg1: Int = arg1
  def getArg2: Int = arg2

object O extends Foo[Int](
  arg2 = { println("arg2"); 3 },
  value = {
    println("value")
    val x = 1
    x
  }
)

@main def Test =
  println(O.getValue)
  println(O.getArg1)
  println(O.getArg2)
