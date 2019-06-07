object PhantomEq {
  import EqUtil._

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

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U) given erased (ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit erased def eqString: PhantomEqEq[String] = ???
  implicit erased def eqInt: PhantomEqEq[Int]       = ???
  implicit erased def eqDouble: PhantomEqEq[Double] = ???

  implicit erased def eqByteNum: PhantomEq[Byte, Number] = ???
  implicit erased def eqNumByte: PhantomEq[Number, Byte] = ???

  implicit erased def eqSeq[T, U] given erased (eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = ???
}
