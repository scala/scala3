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
    def ===[U] (y: U)(implicit ghost ce: PhantomEq[T, U]) = x.equals(y)
  }

  implicit ghost def eqString: PhantomEqEq[String] = ???
  implicit ghost def eqInt: PhantomEqEq[Int]       = ???
  implicit ghost def eqDouble: PhantomEqEq[Double] = ???

  implicit ghost def eqByteNum: PhantomEq[Byte, Number] = ???
  implicit ghost def eqNumByte: PhantomEq[Number, Byte] = ???

  implicit ghost def eqSeq[T, U](implicit ghost eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = ???
}
