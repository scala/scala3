import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 35, classesWithReachableMethods = 10, reachableMethods = 60)
  def main(args: Array[String]): Unit = {
    implicit def s: String = "hello"
    new Foo
  }
}

class Foo(implicit str: String) {
  println(str)
}
