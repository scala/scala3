package dotty.runtime

import scala.reflect.ClassTag

object Arrays {
  def newGenericArray[T](length: Int)(implicit tag: ClassTag[T]): Array[T] = tag.newArray(length)
  def newRefArray[T](length: Int): Array[T] = ???
  def newByteArray(length: Int): Array[Byte] = ???
  def newShortArray(length: Int): Array[Short] = ???
  def newCharArray(length: Int): Array[Char] = ???
  def newIntArray(length: Int): Array[Int] = ???
  def newLongArray(length: Int): Array[Long] = ???
  def newFloatArray(length: Int): Array[Float] = ???
  def newDoubleArray(length: Int): Array[Double] = ???
  def newBooleanArray(length: Int): Array[Boolean] = ???
  def newUnitArray(length: Int): Array[Unit] = ???
}