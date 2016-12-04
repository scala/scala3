import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 92, classesWithReachableMethods = 9, reachableMethods = 59)
  def main(args: Array[String]): Unit = {
    println(new Foo)
  }
}

class Foo {
  override def toString: String = "foo"
}
