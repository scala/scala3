package tests
package hkts


class Case1Variant1[List[_]]
class Case1Variant2[List <: ([_] =>> Any)]

type Case2Variant1 = [A, _] =>> List[A]
type Case2Variant2[A, _] = List[A]

type Case3Variant1 = [_] =>> Int
type Case3Variant2[_] = Int

class A1[X]
class A2[X] extends A1[X]
type Case4Variant1 >: ([X] =>> A2[X]) <: ([X] =>> A1[X])
type Case4Variant2[X] >: A2[X] <: A1[X]

type Case5Variant1 = [X] =>> [Y] =>> (X, Y)
type Case5Variant2[X, Y] = (X, Y)

type Case6Variant1 = [A, B] =>> A => B
type Case6Variant2[A, B] = A => B

class Case7Variant1[List[_] <: Number]
class Case7Variant2[List <: ([_] =>> Number)]

class Case8Variant1[A, List[A] <: Number]
class Case8Variant2[A, List <: ([A] =>> Number)]

type Case9Variant1 <: [X] =>> Option[X]
type Case9Variant2[+X] <: Option[X]

class Case10Variant1[List[_ >: Number]]
class Case10Variant2[List <: ([_ >: Number] =>> Any)]

class Case11Variant1[Map[_, _]]
class Case11Variant2[Map <: ([_, _] =>> Any)]

class Case12Variant1[List[A >: Number]]
class Case12Variant2[List <: ([A >: Number] =>> Any)]

class Case13[List[List[_]]]

trait Case14[C[_]]
class SomeClass extends Case14[List]


def method1[E, T](value: List[? >: E]): Int = 0
def method2[F[+X] <: Option[X], A](fa: F[A]): A = fa.get

import scala.collection.immutable.ArraySeq
trait SomeTraitWithHKTs[A[_], B[_], C[_], D[_], E[_]]
abstract class Foo[Z]
trait SomeTrait extends SomeTraitWithHKTs[Foo, scala.collection.immutable.ArraySeq, ArraySeq, List, [B] =>> List[B]]

trait A[X] {
  type SomeType[X]
  def x: SomeType[X]
}

trait B[D] extends A[D]
