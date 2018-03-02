
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil {

  final class PhantomEq[-L, -R] private[EqUtil]()
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ghost ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit ghost def eqString: PhantomEqEq[String] = new PhantomEq[String, String]
  implicit ghost def eqInt: PhantomEqEq[Int]       = new PhantomEq[Int, Int]
  implicit ghost def eqDouble: PhantomEqEq[Double] = new PhantomEq[Double, Double]

  implicit ghost def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit ghost def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit ghost def eqSeq[T, U](implicit ghost eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] =
    new PhantomEq[Seq[T], Seq[U]]
}
