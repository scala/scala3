// Test case for Array type handling in jsig
// jsig matches: case defn.ArrayOf(elemtp)

import scala.reflect.ClassTag

class ArrayTypesTest:
  // Simple array of primitives
  def arrayOfInt(): Array[Int] = Array(1, 2, 3)

  def processIntArray(arr: Array[Int]): Int = arr.length

  // Array of objects
  def arrayOfString(): Array[String] = Array("a", "b")

  def processStringArray(arr: Array[String]): String =
    arr.mkString(",")

  // Nested arrays
  def arrayOfArrays(): Array[Array[Int]] =
    Array(Array(1), Array(2))

  def processNestedArrays(arr: Array[Array[Int]]): Int =
    arr.head.head

  // Array of generic type
  def arrayOfList(): Array[List[Int]] =
    Array(List(1, 2), List(3, 4))

  def processGenericArray(arr: Array[List[Int]]): Int =
    arr.head.length

  // Array with type parameter
  def genericArray[T: ClassTag](elements: T*): Array[T] =
    elements.toArray[T]

  def useGenericArray(): Array[String] =
    genericArray("a", "b", "c")

  // Array in method parameter
  def sumArray(arr: Array[Int]): Int =
    arr.sum

  // Multidimensional array
  def matrix(): Array[Array[Double]] =
    Array(Array(1.0, 2.0), Array(3.0, 4.0))

  // Array return type with by-name parameter
  def lazyArray(f: => Int): Array[Int] =
    Array(f, f)

  // Array of union type
  def arrayOfUnion(): Array[Int | String] =
    Array(1, "hello", 2)

class Main:
  def main(args: Array[String]): Unit = {
    val test = new ArrayTypesTest
    test.lazyArray(3 + 3)
  }
