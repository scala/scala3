
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
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
