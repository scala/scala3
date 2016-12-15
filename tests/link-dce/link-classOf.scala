import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 89, classesWithReachableMethods = 8, reachableMethods = 56)
  def main(args: Array[String]): Unit = {
    classOf[Foo]
  }
}

class Foo
class Bar
