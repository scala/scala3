import java.util
import java.util.Observable

import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
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

  @scala.export def entryPoint(): Unit = {
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
