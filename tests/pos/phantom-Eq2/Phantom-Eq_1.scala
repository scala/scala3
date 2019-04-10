
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil {

  final class PhantomEq[-L, -R] private[EqUtil]()
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U) given erased (ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit erased def eqString: PhantomEqEq[String] = new PhantomEq[String, String]
  implicit erased def eqInt: PhantomEqEq[Int]       = new PhantomEq[Int, Int]
  implicit erased def eqDouble: PhantomEqEq[Double] = new PhantomEq[Double, Double]

  implicit erased def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit erased def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit erased def eqSeq[T, U] given erased (eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] =
    new PhantomEq[Seq[T], Seq[U]]
}
