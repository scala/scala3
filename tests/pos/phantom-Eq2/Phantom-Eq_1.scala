import scala.language.experimental.erasedDefinitions
import scala.annotation.publicInBinary

/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil {

  final class PhantomEq[-L, -R] @publicInBinary private[EqUtil]()
  type PhantomEqEq[T] = PhantomEq[T, T]

  extension [T](x: T)
    def ===[U] (y: U) (using erased PhantomEq[T, U]) = x.equals(y)

  inline given eqString: PhantomEqEq[String] = new PhantomEq[String, String]
  inline given eqInt: PhantomEqEq[Int]       = new PhantomEq[Int, Int]
  inline given eqDouble: PhantomEqEq[Double] = new PhantomEq[Double, Double]
  inline given eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  inline given eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]
  inline given eqSeq: [T, U] => (erased eq: PhantomEq[T, U]) => PhantomEq[Seq[T], Seq[U]] =
    new PhantomEq[Seq[T], Seq[U]]
}
