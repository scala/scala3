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
   *
   *  @tparam T the element type of the array to create
   *  @param length the number of elements in the new array
   *  @param tag the `ClassTag` providing the runtime class information for `T`
   *  @return the newly created array of type `T`
   */
  def newGenericArray[T](length: Int)(implicit tag: ClassTag[T]): Array[T] =
    tag.newArray(length)

  /** Converts a sequence to a Java array with element type given by `clazz`.
   *
   *  @tparam T the element type of the sequence and resulting array
   *  @param xs the sequence to convert to an array
   *  @param clazz the runtime `Class` of the element type `T`
   *  @return a new array containing the elements of `xs`
   */
  def seqToArray[T](xs: Seq[T], clazz: Class[?]): Array[T] = {
    val arr = java.lang.reflect.Array.newInstance(clazz, xs.length).asInstanceOf[Array[T]]
    xs.copyToArray(arr)
    arr
  }

  /** Creates an array of a reference type T.
   *
   *  @tparam Arr the type of the resulting array, which may be multi-dimensional (e.g., `Array[Array[Int]]`)
   *  @param componentType the runtime `Class` of the array's component type
   *  @param returnType the `Class` representing the return type `Arr` (unused at runtime, provides type information for the compiler)
   *  @param dimensions the sizes for each dimension of the new array
   *  @return the newly created (possibly multi-dimensional) array
   */
  def newArray[Arr](componentType: Class[?], @unused returnType: Class[Arr], dimensions: Array[Int]): Arr =
    jlr.Array.newInstance(componentType, dimensions*).asInstanceOf[Arr]
}
