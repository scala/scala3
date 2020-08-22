abstract class Coll3[E] extends java.util.Collection[E] {
  override def toArray[T](a: Array[T]): Array[T] = ??? // error: method toArray has a different signature than the overridden declaration
}
