/* This is a example of how to implement Eq using erasable phantom types.
 *
 * See also: ../pos/phantomEq.scala
 */

object PhantomEqTest {
  import EqUtil._

  "abc" === "abc"
  1 === 4

  1 === "abc" // error
  "ghi" === 4 // error
  0 === Nil // error
  List(1, 2) === 1 // error
  List(1, 2) === "" // error

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

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] =
    assume[PhantomEq[Seq[T], Seq[U]]]

}
