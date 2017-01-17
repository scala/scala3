import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 31, classesWithReachableMethods = 10, reachableMethods = 53)
  def main(args: Array[String]): Unit = {
    System.out.println(new Bar)
  }
}

class Bar extends Foo {
  def foo[T <: Foo](t: T): String = t.string()

  def string(): String = "42"

  override def toString(): String = foo(this)
}