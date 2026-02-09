import scala.language.experimental.erasedDefinitions

object PhantomEq {
  import EqUtil.*

  "ghi" === "jkl"
  3 === 4
  2.0 === 3.1

  List(1, 2) === Nil
  List(1, 2) === Vector(1, 2)

  1.toByte === (1: Number)
  (1: Number) === 1.toByte
}

object EqUtil {

  class PhantomEq[-L, -R]
  type PhantomEqEq[T] = PhantomEq[T, T]
  erased val phantomEq = PhantomEq[Any, Any]()

  extension [T](x: T)
    def ===[U](y: U)(using erased PhantomEq[T, U]) = x.equals(y)

  inline given eqString: PhantomEqEq[String] = phantomEq
  inline given eqInt: PhantomEqEq[Int]       = phantomEq
  inline given eqDouble: PhantomEqEq[Double] = phantomEq

  inline given eqByteNum: PhantomEq[Byte, Number] = phantomEq
  inline given eqNumByte: PhantomEq[Number, Byte] = phantomEq

  inline given eqSeq: [T, U] => (erased PhantomEq[T, U]) => PhantomEq[Seq[T], Seq[U]] = phantomEq
}
