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

  erased given eqString: PhantomEqEq[String]
  erased given eqInt: PhantomEqEq[Int]
  erased given eqDouble: PhantomEqEq[Double]

  erased given eqByteNum: PhantomEq[Byte, Number]
  erased given eqNumByte: PhantomEq[Number, Byte]

  erased given eqSeq[T, U](using erased PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]]
}
