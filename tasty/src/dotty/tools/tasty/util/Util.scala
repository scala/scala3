package dotty.tools.tasty.util

import reflect.ClassTag

object Util {

  /** An array twice the size of given array, with existing elements copied over */
  def dble[T: ClassTag](arr: Array[T]): Array[T] = {
    val arr1 = new Array[T](arr.length * 2)
    System.arraycopy(arr, 0, arr1, 0, arr.length)
    arr1
  }

  /** Specialized version for bytes */
  def dble(arr: Array[Byte]): Array[Byte] = {
    val arr1 = new Array[Byte](arr.length * 2)
    System.arraycopy(arr, 0, arr1, 0, arr.length)
    arr1
  }

  /** Specialized version for ints */
  def dble(arr: Array[Int]): Array[Int] = {
    val arr1 = new Array[Int](arr.length * 2)
    System.arraycopy(arr, 0, arr1, 0, arr.length)
    arr1
  }
}
