import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 44)
  def main(args: Array[String]): Unit = {
    System.out.println(new Bar)
  }
}

class Bar extends Foo {
  def foo[T <: Foo](t: T): String = t.string()

  def string(): String = "42"

  override def toString(): String = foo(this)
}