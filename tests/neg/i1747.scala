class Coll[E] extends java.util.Collection[E] {
  def toArray[T](a: Array[T]): Array[T] = ??? // error: cannot override // error
}
