package tests
package contextBounds

import scala.reflect.ClassTag

class A:
  def basic[A : ClassTag]: A
    = ???

  def basic2[A : ClassTag, B : List]: A
    = ???

  trait Build[X, Y]
  trait From[A, B]
  def b[T : ([T] =>> Build[From[T, T], T])](t: T): T
    = t

  trait Build2[X[_], Y]
  trait From2[A, B]

  def b2[T : ([T] =>> Build2[[Y] =>> From2[T, Y], T])](t: T): T
    = t

  // Tests not support multiline signatures
  def a[T <: String | Int : ([T] =>> T match { case String => A case Int => B })](t: T): T
    = t

  def falsePositive[T](evidence$1: ClassTag[T]): Int 
    = 1

  // Scala spec stats that behaviour of names with `$` is undefined.
  // Scaladoc documents definition below as `def falsePositive2[T: ClassTag]: Int`
  // that is equivalent of methods below
  // def falsePositive2[T](implicit evidence$3: ClassTag[T]): Int 
  //   = 1

  class Outer[A]:
    def falsePositiveInner[T](implicit evidence$3: ClassTag[A]): Int 
      = 1