import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 8, reachableMethods = 45)
  def main(args: Array[String]): Unit = {
    System.out.println(new Bar)
  }
}

class Bar extends Foo {
  def foo[T](t: T): T = t

  override def toString(): String = foo("42")
}