import scala.annotation.internal

object Test {
  class A[T] {
    def f(y: T): T = ((x: T) => x)(y)
  }

  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 8, reachableMethods = 9)
  def main(args: Array[String]): Unit = System.out.println((new A[Int]).f(42))
}