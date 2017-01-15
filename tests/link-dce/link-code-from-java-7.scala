import scala.annotation.internal

import java.lang.InheritableThreadLocal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 27, classesWithReachableMethods = 10, reachableMethods = 53)
  def main(args: Array[String]): Unit = {
    val a = new DynamicVariable2(42)
    System.out.println(a.value)
  }
}


class DynamicVariable2[T](init: T) {
  private val tl = new InheritableThreadLocal[T] {
    override def initialValue = init.asInstanceOf[T with AnyRef]
  }

  def value: T = tl.get.asInstanceOf[T]
}
