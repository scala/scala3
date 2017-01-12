import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 10, classesWithReachableMethods = 4, reachableMethods = 6)
  def main(args: Array[String]): Unit = {
    new Foo
  }
}

final class Foo extends Bar[Int]

trait Bar[T] {
  val value: Int = 42
}
