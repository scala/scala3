abstract class Coll[E] extends java.util.Collection[E] {
  override def toArray[T](a: Array[T]): Array[T] = ??? // error: has different signature
}
