
import java.lang.InheritableThreadLocal

import scala.annotation.internal


object Test {
//  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 6, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    System.out.println(new DynamicVariableFoo[String]("42").value)
  }
}


class DynamicVariableFoo[T](init: T) {
  private val tl = new InheritableThreadLocal[T] {
    override def initialValue = init.asInstanceOf[T with AnyRef]
  }

  def value: T = tl.get.asInstanceOf[T]
}
