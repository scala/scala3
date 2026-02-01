abstract class Foo(a: => Int, b: Int, c: Int)

object Bar extends Foo(
  b = { println("b evaluated"); 2 },
  c = { println("c evaluated"); 3 },
  a = { println("a evaluated"); 1 }
)

object Test:
  def main(args: Array[String]): Unit =
    Bar  // trigger initialization
