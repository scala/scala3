class Coll[E] extends java.util.Collection[E] { // error: needs to be abstract
  def toArray[T](a: Array[T]): Array[T] = ??? // error: cannot override
}
