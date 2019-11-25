package dotty.tools.tasty.util

import reflect.ClassTag

object Util {

  /** An array twice the size of given array, with existing elements copied over */
  def dble[T: ClassTag](arr: Array[T]): Array[T] = {
    val arr1 = new Array[T](arr.length * 2)
    System.arraycopy(arr, 0, arr1, 0, arr.length)
    arr1
  }

}
