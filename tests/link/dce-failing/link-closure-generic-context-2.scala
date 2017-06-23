import scala.annotation.internal

object Test {
  class A[T] {
    def f(y: T): T = ((x: T) => x)(y)
  }
  
  class B[T] extends A[T]

  // @internal.link.CallGraphBounds(reachableClasses = 1, classesWithReachableMethods = 1, reachableMethods = 1)
  def main(args: Array[String]): Unit = System.out.println((new B[Int]).f(42))
}