import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 31, classesWithReachableMethods = 7, reachableMethods = 55)
  def main(args: Array[String]): Unit = {
    classOf[Foo]
  }
}

class Foo
class Bar
