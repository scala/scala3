// Test evaluation order with by-name parameters and named arguments in constructor
abstract class Foo(a: => Unit, b: Int, c: Int):
  def triggerA(): Unit = a
  def getB: Int = b
  def getC: Int = c

object Bar extends Foo(
  c = { println("c evaluated"); 2 },
  a = { println("a evaluated") },
  b = { println("b evaluated"); 1 }
):
  println("Bar body")
  println(s"b = $getB")
  println(s"c = $getC")
  triggerA()
  triggerA() // should evaluated again

@main def Test =
  Bar
