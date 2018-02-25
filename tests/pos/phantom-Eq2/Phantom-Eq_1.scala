
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil {

  final class PhantomEq[-L, -R] private[EqUtil]()
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit unused ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit unused def eqString: PhantomEqEq[String] = new PhantomEq[String, String]
  implicit unused def eqInt: PhantomEqEq[Int]       = new PhantomEq[Int, Int]
  implicit unused def eqDouble: PhantomEqEq[Double] = new PhantomEq[Double, Double]

  implicit unused def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit unused def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit unused def eqSeq[T, U](implicit unused eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] =
    new PhantomEq[Seq[T], Seq[U]]
}
