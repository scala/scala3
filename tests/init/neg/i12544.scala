enum Enum:
  case Case
  case Case2(x: Int)

def g(b: Enum.B): Int = b.foo()

object Enum:
  object nested:               // error
    val a: Enum = Case

  val b: Enum = f(nested.a)

  def f(e: Enum): Enum = e

  class B() { def foo() = n + 1 }
  g(new B())                       // error
  val n: Int = 10

@main def main(): Unit = println(Enum.b)
