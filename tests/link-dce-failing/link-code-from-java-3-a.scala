import java.util.Observable

import scala.annotation.internal

object Test {
  // @internal.link.CallGraphBounds(reachableClasses = 1, classesWithReachableMethods = 1, reachableMethods = 1)
  def main(args: Array[String]): Unit = {
    val classLoader = Test.getClass.getClassLoader()

    try {
      val mainClass = classLoader.loadClass("Test")
      val mainMethod = mainClass.getMethod("dceTest")
      mainMethod.invoke(null);
    } catch {
      case e: java.lang.Exception => e.getCause.printStackTrace()
    }
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def shouldDCE(expr: => Any): Unit = try {
    expr
    throw new Exception("Expected DCE")
  } catch {
    case dce: dotty.runtime.DeadCodeEliminated =>
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")

    val a = new A[AA]
    a.hasNext
    a.next()
    val aa = new AA
    Test.shouldDCE(aa.f)
    aa.toString
  }

  @scala.EntryPoint def entryPoint(): Unit = {
    System.out.print(new A[AA])
  }
}

class AA {
  def f = 42
  override def toString: String = super.toString
}

class A[E] extends java.util.Iterator[E] {
  override def next(): E = null.asInstanceOf[E]
  override def hasNext: Boolean = false
}
