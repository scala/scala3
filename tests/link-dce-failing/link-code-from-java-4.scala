import java.util
import java.util.Observable

import scala.annotation.internal

object Test {
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
    val coll = new Coll[AA]
    coll.removeAll(coll)
    coll.retainAll(coll)
    coll.clear()
    coll.toArray
    coll.toArray(new Array[AA](0))
    coll.size()
    coll.remove(new AA)
    coll.contains(new AA)
    coll.addAll(coll)
    coll.iterator()
    coll.isEmpty
    coll.containsAll(coll)
    coll.add(new AA)

    val aa = new AA
    Test.shouldDCE(aa.f)
    aa.toString
  }

  @scala.EntryPoint def entryPoint(): Unit = {
    System.out.print(new Coll[AA])
  }
}

class AA {
  def f = 42
  override def toString: String = super.toString
}

class Coll[E] extends java.util.Collection[E] {
  def removeAll(c: util.Collection[_]): Boolean = false

  def retainAll(c: util.Collection[_]): Boolean = false

  def clear(): Unit = ()

  def toArray: Array[Object] = null

  def toArray[T](a: Array[T]): Array[T] = null

  def size(): Int = 0

  def remove(o: scala.Any): Boolean = false

  def contains(o: scala.Any): Boolean = false

  def addAll(c: util.Collection[_ <: E]): Boolean = false

  def iterator(): util.Iterator[E] = null

  def isEmpty: Boolean = false

  def containsAll(c: util.Collection[_]): Boolean = false

  def add(e: E): Boolean = false
}
