
import Macros._

object Test {
  type A
  type B <: A

  def main(args: Array[String]): Unit = {
    // true
    println(isTypeEqual[Int, Int])
    println(isTypeEqual[String, String])
    println(isTypeEqual[Test.type, Test.type])
    println(isTypeEqual[2, 2])
    println(isTypeEqual[A, A])
    println()

    // false
    println(isTypeEqual[Int, Double])
    println(isTypeEqual[String, Int])
    println(isTypeEqual[Test.type, Macros.type])
    println(isTypeEqual[2, 1])
    println(isTypeEqual[A, B])
    println()

    // true
    println(isSubTypeOf[Int, Int])
    println(isSubTypeOf[String, Object])
    println(isSubTypeOf[Int, AnyVal])
    println(isSubTypeOf[AnyRef, Any])
    println(isSubTypeOf[AnyVal, Any])
    println(isSubTypeOf[B, A])
    println()

    // false
    println(isSubTypeOf[Object, Int])
    println(isSubTypeOf[AnyVal, Int])
    println(isSubTypeOf[Any, AnyRef])
    println(isSubTypeOf[Any, AnyVal])
    println(isSubTypeOf[A, B])
  }
}
