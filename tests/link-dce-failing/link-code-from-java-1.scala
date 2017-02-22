import java.io.File
import java.io.FileFilter
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Type

import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 139, classesWithReachableMethods = 23, reachableMethods = 85)
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

    val a = new A
    Test.shouldDCE(a.f1)
    a.hashCode()
    a.equals(a)
    a.toString

    val b = new B {}
    Test.shouldDCE(b.f1)
    b.hashCode()
    b.equals(b)
    b.toString

    val c = new C {}
    Test.shouldDCE(c.f1)
    c.hashCode()
    c.equals(c)
    c.toString
  }

  @EntryPoint def entryPoint(): Unit = {
    System.out.println(new A)
    System.out.println(new B {})
    System.out.println(new C {})
  }
}

class A {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}

abstract class B {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}

trait C {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}
