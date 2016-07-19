
/* This is a example of how to implement Eq using erasable phantom types.
 *
 * Run this test with
 *   `run tests/pos/phantomEq.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * See also: ../neg/phantomEq.scala
 */

object PhantomEq {
  import EqUtil._

  "ghi" === "jkl"
  3 === 4
  2.0 === 3.1

  List(1, 2) === Nil
  List(1, 2) === Vector(1, 2)

  1.toByte === (1: Number)
  (1: Number) === 1.toByte
}

object EqUtil extends Phantom {

  type PhantomEq[-L, -R] <: this.Any
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit def eqString: PhantomEqEq[String] = assume[PhantomEqEq[String]]
  implicit def eqInt: PhantomEqEq[Int]       = assume[PhantomEqEq[Int]]
  implicit def eqDouble: PhantomEqEq[Double] = assume[PhantomEqEq[Double]]

  implicit def eqByteNum: PhantomEq[Byte, Number] = assume[PhantomEq[Byte, Number]]
  implicit def eqNumByte: PhantomEq[Number, Byte] = assume[PhantomEq[Number, Byte]]

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = assume[PhantomEq[Seq[T], Seq[U]]]
}
