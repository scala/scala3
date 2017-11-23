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
    def ===[U] (y: U)(implicit unused ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit unused def eqString: PhantomEqEq[String] = assume
  implicit unused def eqInt: PhantomEqEq[Int]       = assume
  implicit unused def eqDouble: PhantomEqEq[Double] = assume

  implicit unused def eqByteNum: PhantomEq[Byte, Number] = assume
  implicit unused def eqNumByte: PhantomEq[Number, Byte] = assume

  implicit unused def eqSeq[T, U](implicit unused eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = assume
}
