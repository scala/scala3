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
    // TODO: check stack trace to see if the DCE was in the fist call of expr
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")

    val b = new B[BB] {}
    b.hasNext
    b.next()
    val bb = new BB
    Test.shouldDCE(bb.f)
    bb.toString
  }

  @scala.EntryPoint def entryPoint(): Unit = {
    System.out.print(new B[BB] {})
  }
}

class BB {
  def f = 43
  override def toString: String = super.toString
}

abstract class B[E] extends java.util.Iterator[E] {
  override def next(): E = null.asInstanceOf[E]
  override def hasNext(): Boolean = false
}
