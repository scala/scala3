
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil extends Phantom {

  type PhantomEq[-L, -R] <: this.Any
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit def eqString: PhantomEqEq[String] = assume
  implicit def eqInt: PhantomEqEq[Int]       = assume
  implicit def eqDouble: PhantomEqEq[Double] = assume

  implicit def eqByteNum: PhantomEq[Byte, Number] = assume
  implicit def eqNumByte: PhantomEq[Number, Byte] = assume

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = assume
}
