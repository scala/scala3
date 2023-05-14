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

  type PhantomEq[-L, -R]
  type PhantomEqEq[T] = PhantomEq[T, T]

  extension [T](x: T)
    def ===[U](y: U)(using erased PhantomEq[T, U]) = x.equals(y)

  erased given eqString: PhantomEqEq[String] = compiletime.erasedValue
  erased given eqInt: PhantomEqEq[Int]       = compiletime.erasedValue
  erased given eqDouble: PhantomEqEq[Double] = compiletime.erasedValue

  erased given eqByteNum: PhantomEq[Byte, Number] = compiletime.erasedValue
  erased given eqNumByte: PhantomEq[Number, Byte] = compiletime.erasedValue

  erased given eqSeq[T, U](using erased PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = compiletime.erasedValue
}
