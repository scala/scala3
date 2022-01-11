
/* This is a version of ../pos/phantomEq.scala that tests phantom with separate compilation */
object EqUtil {

  final class PhantomEq[-L, -R] private[EqUtil]()
  type PhantomEqEq[T] = PhantomEq[T, T]

  extension [T](x: T)
    def ===[U] (y: U) (using erased PhantomEq[T, U]) = x.equals(y)

  erased given eqString: PhantomEqEq[String]
  erased given eqInt: PhantomEqEq[Int]
  erased given eqDouble: PhantomEqEq[Double]
  erased given eqByteNum: PhantomEq[Byte, Number]
  erased given eqNumByte: PhantomEq[Number, Byte]
  erased given eqSeq[T, U] (using erased eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]]
}
