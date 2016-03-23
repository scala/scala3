package dotty.runtime

import scala.reflect.ClassTag

import java.lang.{reflect => jlr}

/** All but the first two operations should be short-circuited and implemented specially by
 *  the backend.
 */
object Arrays {

  // note: this class is magical. Do not touch it unless you know what you are doing.`

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

  /** Create an array of a reference type T.
   */
  def newRefArray[T](componentType: Class[T])(length: Int): Array[T] =
    jlr.Array.newInstance(componentType, length).asInstanceOf[Array[T]]

  /** Create a Byte[] array */
  def newByteArray(length: Int): Array[Byte] =
    jlr.Array.newInstance(classOf[Byte], length).asInstanceOf[Array[Byte]]

  /** Create a Short[] array */
  def newShortArray(length: Int): Array[Short] =
    jlr.Array.newInstance(classOf[Short], length).asInstanceOf[Array[Short]]

  /** Create a Char[] array */
  def newCharArray(length: Int): Array[Char] =
    jlr.Array.newInstance(classOf[Char], length).asInstanceOf[Array[Char]]

  /** Create an Int[] array */
  def newIntArray(length: Int): Array[Int] =
    jlr.Array.newInstance(classOf[Int], length).asInstanceOf[Array[Int]]

  /** Create a Long[] array */
  def newLongArray(length: Int): Array[Long] =
    jlr.Array.newInstance(classOf[Long], length).asInstanceOf[Array[Long]]

  /** Create a Float[] array */
  def newFloatArray(length: Int): Array[Float] =
    jlr.Array.newInstance(classOf[Float], length).asInstanceOf[Array[Float]]

  /** Create a Double[] array */
  def newDoubleArray(length: Int): Array[Double] =
    jlr.Array.newInstance(classOf[Double], length).asInstanceOf[Array[Double]]

  /** Create a Boolean[] array */
  def newBooleanArray(length: Int): Array[Boolean] =
    jlr.Array.newInstance(classOf[Boolean], length).asInstanceOf[Array[Boolean]]

  /** Create a scala.runtime.BoxedUnit[] array */
  def newUnitArray(length: Int): Array[Unit] =
    jlr.Array.newInstance(classOf[scala.runtime.BoxedUnit], length).asInstanceOf[Array[Unit]]
}
