package dotty.runtime

import scala.reflect.ClassTag

/** All but the first two operations should be short-circuited and implemented specially by
 *  the backend.
 */
object Arrays {

  /** Creates an array of some element type determined by the given `ClassTag`
   *  argument. The erased type of applications of this method is `Object`.
   */
  def newGenericArray[T](length: Int)(implicit tag: ClassTag[T]): Array[T] =
    tag.newArray(length)

  /** Convert a sequence to a Java array with element type given by `clazz`. */
  def seqToArray[T](xs: Seq[T], clazz: Class[_]): Array[T] = {
    val arr = java.lang.reflect.Array.newInstance(clazz, xs.length).asInstanceOf[Array[T]]
    xs.copyToArray(arr)
    arr
  }

  /** Create an array of type T. T must be of form Array[E], with
   *  E being a reference type.
   */
  def newRefArray[T](length: Int): T = ???

  /** Create a Byte[] array */
  def newByteArray(length: Int): Array[Byte] = ???

  /** Create a Short[] array */
  def newShortArray(length: Int): Array[Short] = ???

  /** Create a Char[] array */
  def newCharArray(length: Int): Array[Char] = ???

  /** Create an Int[] array */
  def newIntArray(length: Int): Array[Int] = ???

  /** Create a Long[] array */
  def newLongArray(length: Int): Array[Long] = ???

  /** Create a Float[] array */
  def newFloatArray(length: Int): Array[Float] = ???

  /** Create a Double[] array */
  def newDoubleArray(length: Int): Array[Double] = ???

  /** Create a Boolean[] array */
  def newBooleanArray(length: Int): Array[Boolean] = ???

  /** Create a scala.runtime.BoxedUnit[] array */
  def newUnitArray(length: Int): Array[Unit] = ???
}
