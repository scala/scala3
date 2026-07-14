abstract class Coll1[E] extends java.util.Collection[E] {
  override def toArray[T](a: Array[T & Object]): Array[T & Object] = ???
}

abstract class Coll2[E] extends java.util.Collection[E] {
  override def toArray[T <: Object](a: Array[T]): Array[T] = ???
}
