package scala.runtime

import language.experimental.captureChecking

import scala.annotation.unused
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

  /** Converts a sequence to a Java array with element type given by `clazz`. */
  def seqToArray[T](xs: Seq[T], clazz: Class[?]): Array[T] = {
    val arr = java.lang.reflect.Array.newInstance(clazz, xs.length).asInstanceOf[Array[T]]
    xs.copyToArray(arr)
    arr
  }

  /** Creates an array of a reference type T.
   */
  def newArray[Arr](componentType: Class[?], @unused returnType: Class[Arr], dimensions: Array[Int]): Arr =
    jlr.Array.newInstance(componentType, dimensions*).asInstanceOf[Arr]
}
