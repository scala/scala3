import java.util
object Test {
  def main(args: Array[String]): Unit = {
    println(new Coll[Int].toArray(Array(1, 2, 3)).mkString)
  }
}

class Coll[E] extends java.util.Collection[E] {

  def toArray[T](a: Array[T]): Array[T] = {
    a(0) = null.asInstanceOf[T]
    a
  }

  def removeAll(c: util.Collection[_]): Boolean = ???
  def retainAll(c: util.Collection[_]): Boolean = ???
  def clear(): Unit = ???
  def toArray: Array[AnyRef] = ???
  def size(): Int = ???
  def remove(o: scala.Any): Boolean = ???
  def contains(o: scala.Any): Boolean = ???
  def addAll(c: util.Collection[_ <: E]): Boolean = ???
  def iterator(): util.Iterator[E] = ???
  def isEmpty: Boolean = ???
  def containsAll(c: util.Collection[_]): Boolean = ???
  def add(e: E): Boolean = ???
}