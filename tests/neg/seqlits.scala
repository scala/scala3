import language.`3.7`
import compiletime.ExpressibleAsCollectionLiteral
import language.experimental.collectionLiterals
import collection.immutable.BitSet

class A

case class B(xs: Int*) extends A
case class C(xs: Int*) extends A

class D

object SeqLits:

  given [T] => ExpressibleAsCollectionLiteral[B]:
    type Elem = Int
    inline def fromLiteral(inline xs: Int*): B = B(xs*)

  given [T] => ExpressibleAsCollectionLiteral[C]:
    type Elem = Int
    inline def fromLiteral(inline xs: Int*): C = C(xs*)

  val x: A = [1, 2, 3] // error: ambiguous
  val y: D = [1, 2, 3] // error: type mismatch

  val mbss: Map[BitSet, Seq[Int]] =  [[1] -> [1], [0, 2] -> [1, 2], [0] -> []] // error: type mismatch // error // error

  def f[A](xs: List[A]) = xs
  def f[A](xs: Vector[A]) = xs
  val xx = f([1, 2, 3])  // error: no matching alternatives for Seq[Int]

  def g[A](xs: Set[A]): Set[A] = xs
  def g[A](xs: collection.immutable.HashSet[A]): Set[A] = xs
  val yy = g([1, 2, 3])  // error: no matching alternatives for Seq[Int] (even if one method is more specific than the other)


