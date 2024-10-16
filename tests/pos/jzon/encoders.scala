trait Meta[T]
trait Lazy[T]{
  lazy val value: T = ???
}
trait Encoder[A] { self =>
  def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = ???
}
abstract class SealedTraitEncoder[A, ST <: SealedTrait[A]](subs: Array[Meta[?]]) extends Encoder[ST] {
  def unsafeEncodeValue(st: ST, indent: Option[Int], out: java.io.Writer): Unit
  final override def unsafeEncode(st: ST, indent: Option[Int], out: java.io.Writer): Unit = ???
}
abstract class SealedTraitDiscrimEncoder[A, ST <: SealedTrait[A]](
  subs: Array[Meta[?]],
  hintfield: String
) extends Encoder[ST] {
  def unsafeEncodeValue(st: ST, indent: Option[Int], out: java.io.Writer): Unit
  final override def unsafeEncode(st: ST, indent: Option[Int], out: java.io.Writer): Unit = ???
}

object Encoders{
 implicit def sealedtrait1[A, A1 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]]): Encoder[SealedTrait1[A, A1]] = {
    def work(st: SealedTrait1[A, A1], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait1[A, A1]](Array(M1)) {
        override def unsafeEncodeValue(st: SealedTrait1[A, A1], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait1[A, A1]](Array(M1), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait1[A, A1], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait2[A, A1 <: A, A2 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]]): Encoder[SealedTrait2[A, A1, A2]] = {
    def work(st: SealedTrait2[A, A1, A2], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait2[A, A1, A2]](Array(M1, M2)) {
        override def unsafeEncodeValue(st: SealedTrait2[A, A1, A2], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait2[A, A1, A2]](Array(M1, M2), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait2[A, A1, A2], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait3[A, A1 <: A, A2 <: A, A3 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]]): Encoder[SealedTrait3[A, A1, A2, A3]] = {
    def work(st: SealedTrait3[A, A1, A2, A3], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait3[A, A1, A2, A3]](Array(M1, M2, M3)) {
        override def unsafeEncodeValue(st: SealedTrait3[A, A1, A2, A3], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait3[A, A1, A2, A3]](Array(M1, M2, M3), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait3[A, A1, A2, A3], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait4[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]]): Encoder[SealedTrait4[A, A1, A2, A3, A4]] = {
    def work(st: SealedTrait4[A, A1, A2, A3, A4], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait4[A, A1, A2, A3, A4]](Array(M1, M2, M3, M4)) {
        override def unsafeEncodeValue(st: SealedTrait4[A, A1, A2, A3, A4], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait4[A, A1, A2, A3, A4]](Array(M1, M2, M3, M4), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait4[A, A1, A2, A3, A4], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait5[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]]): Encoder[SealedTrait5[A, A1, A2, A3, A4, A5]] = {
    def work(st: SealedTrait5[A, A1, A2, A3, A4, A5], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait5[A, A1, A2, A3, A4, A5]](Array(M1, M2, M3, M4, M5)) {
        override def unsafeEncodeValue(st: SealedTrait5[A, A1, A2, A3, A4, A5], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait5[A, A1, A2, A3, A4, A5]](Array(M1, M2, M3, M4, M5), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait5[A, A1, A2, A3, A4, A5], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait6[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]]): Encoder[SealedTrait6[A, A1, A2, A3, A4, A5, A6]] = {
    def work(st: SealedTrait6[A, A1, A2, A3, A4, A5, A6], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait6[A, A1, A2, A3, A4, A5, A6]](Array(M1, M2, M3, M4, M5, M6)) {
        override def unsafeEncodeValue(st: SealedTrait6[A, A1, A2, A3, A4, A5, A6], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait6[A, A1, A2, A3, A4, A5, A6]](Array(M1, M2, M3, M4, M5, M6), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait6[A, A1, A2, A3, A4, A5, A6], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait7[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]]): Encoder[SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7]] = {
    def work(st: SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7]](Array(M1, M2, M3, M4, M5, M6, M7)) {
        override def unsafeEncodeValue(st: SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7]](Array(M1, M2, M3, M4, M5, M6, M7), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait7[A, A1, A2, A3, A4, A5, A6, A7], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait8[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]]): Encoder[SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8]] = {
    def work(st: SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8]](Array(M1, M2, M3, M4, M5, M6, M7, M8)) {
        override def unsafeEncodeValue(st: SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8]](Array(M1, M2, M3, M4, M5, M6, M7, M8), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait8[A, A1, A2, A3, A4, A5, A6, A7, A8], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait9[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]]): Encoder[SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = {
    def work(st: SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9)) {
        override def unsafeEncodeValue(st: SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait9[A, A1, A2, A3, A4, A5, A6, A7, A8, A9], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait10[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]]): Encoder[SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = {
    def work(st: SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10)) {
        override def unsafeEncodeValue(st: SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait10[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait11[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]]): Encoder[SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = {
    def work(st: SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)) {
        override def unsafeEncodeValue(st: SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait11[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait12[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]]): Encoder[SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = {
    def work(st: SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12)) {
        override def unsafeEncodeValue(st: SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait12[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait13[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]]): Encoder[SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = {
    def work(st: SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13)) {
        override def unsafeEncodeValue(st: SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait13[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait14[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]]): Encoder[SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = {
    def work(st: SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14)) {
        override def unsafeEncodeValue(st: SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait14[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait15[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]]): Encoder[SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = {
    def work(st: SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15)) {
        override def unsafeEncodeValue(st: SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait15[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait16[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]]): Encoder[SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = {
    def work(st: SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16)) {
        override def unsafeEncodeValue(st: SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait16[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait17[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]]): Encoder[SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = {
    def work(st: SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17)) {
        override def unsafeEncodeValue(st: SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait17[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait18[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]]): Encoder[SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = {
    def work(st: SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18)) {
        override def unsafeEncodeValue(st: SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait18[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait19[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]]): Encoder[SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = {
    def work(st: SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19)) {
        override def unsafeEncodeValue(st: SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait19[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait20[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]]): Encoder[SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = {
    def work(st: SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20)) {
        override def unsafeEncodeValue(st: SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait20[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait21[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]]): Encoder[SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = {
    def work(st: SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21)) {
        override def unsafeEncodeValue(st: SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait21[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait22[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]]): Encoder[SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = {
    def work(st: SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22)) {
        override def unsafeEncodeValue(st: SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait22[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait23[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]]): Encoder[SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]] = {
    def work(st: SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23)) {
        override def unsafeEncodeValue(st: SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait23[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait24[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]]): Encoder[SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24]] = {
    def work(st: SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24)) {
        override def unsafeEncodeValue(st: SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait24[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait25[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]]): Encoder[SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25]] = {
    def work(st: SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25)) {
        override def unsafeEncodeValue(st: SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait25[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait26[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]]): Encoder[SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26]] = {
    def work(st: SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26)) {
        override def unsafeEncodeValue(st: SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait26[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait27[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]]): Encoder[SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27]] = {
    def work(st: SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27)) {
        override def unsafeEncodeValue(st: SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait27[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait28[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]]): Encoder[SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28]] = {
    def work(st: SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28)) {
        override def unsafeEncodeValue(st: SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait28[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait29[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]]): Encoder[SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29]] = {
    def work(st: SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29)) {
        override def unsafeEncodeValue(st: SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait29[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait30[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]]): Encoder[SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30]] = {
    def work(st: SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30)) {
        override def unsafeEncodeValue(st: SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait30[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait31[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]]): Encoder[SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31]] = {
    def work(st: SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31)) {
        override def unsafeEncodeValue(st: SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait31[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait32[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]]): Encoder[SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32]] = {
    def work(st: SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32)) {
        override def unsafeEncodeValue(st: SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait32[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait33[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]]): Encoder[SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33]] = {
    def work(st: SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33)) {
        override def unsafeEncodeValue(st: SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait33[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait34[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]]): Encoder[SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34]] = {
    def work(st: SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34)) {
        override def unsafeEncodeValue(st: SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait34[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait35[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]]): Encoder[SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35]] = {
    def work(st: SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35)) {
        override def unsafeEncodeValue(st: SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait35[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait36[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]]): Encoder[SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36]] = {
    def work(st: SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36)) {
        override def unsafeEncodeValue(st: SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait36[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait37[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]]): Encoder[SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37]] = {
    def work(st: SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37)) {
        override def unsafeEncodeValue(st: SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait37[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait38[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]]): Encoder[SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38]] = {
    def work(st: SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38)) {
        override def unsafeEncodeValue(st: SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait38[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait39[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]]): Encoder[SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39]] = {
    def work(st: SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39)) {
        override def unsafeEncodeValue(st: SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait39[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait40[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]]): Encoder[SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40]] = {
    def work(st: SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40)) {
        override def unsafeEncodeValue(st: SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait40[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait41[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]]): Encoder[SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41]] = {
    def work(st: SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41)) {
        override def unsafeEncodeValue(st: SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait41[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait42[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]]): Encoder[SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42]] = {
    def work(st: SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42)) {
        override def unsafeEncodeValue(st: SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait42[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait43[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]]): Encoder[SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43]] = {
    def work(st: SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43)) {
        override def unsafeEncodeValue(st: SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait43[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait44[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]]): Encoder[SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44]] = {
    def work(st: SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44)) {
        override def unsafeEncodeValue(st: SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait44[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait45[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]]): Encoder[SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45]] = {
    def work(st: SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45)) {
        override def unsafeEncodeValue(st: SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait45[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait46[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]]): Encoder[SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46]] = {
    def work(st: SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46)) {
        override def unsafeEncodeValue(st: SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait46[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait47[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]]): Encoder[SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47]] = {
    def work(st: SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47)) {
        override def unsafeEncodeValue(st: SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait47[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait48[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]]): Encoder[SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48]] = {
    def work(st: SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48)) {
        override def unsafeEncodeValue(st: SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait48[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait49[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]]): Encoder[SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49]] = {
    def work(st: SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49)) {
        override def unsafeEncodeValue(st: SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait49[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait50[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]]): Encoder[SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50]] = {
    def work(st: SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50)) {
        override def unsafeEncodeValue(st: SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait50[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait51[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]]): Encoder[SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51]] = {
    def work(st: SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51)) {
        override def unsafeEncodeValue(st: SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait51[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait52[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]]): Encoder[SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52]] = {
    def work(st: SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52)) {
        override def unsafeEncodeValue(st: SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait52[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait53[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]]): Encoder[SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53]] = {
    def work(st: SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53)) {
        override def unsafeEncodeValue(st: SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait53[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait54[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]]): Encoder[SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54]] = {
    def work(st: SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54)) {
        override def unsafeEncodeValue(st: SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait54[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait55[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]]): Encoder[SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55]] = {
    def work(st: SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55)) {
        override def unsafeEncodeValue(st: SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait55[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait56[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]]): Encoder[SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56]] = {
    def work(st: SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56)) {
        override def unsafeEncodeValue(st: SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait56[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait57[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]]): Encoder[SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57]] = {
    def work(st: SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57)) {
        override def unsafeEncodeValue(st: SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait57[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait58[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]]): Encoder[SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58]] = {
    def work(st: SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58)) {
        override def unsafeEncodeValue(st: SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait58[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait59[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]]): Encoder[SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59]] = {
    def work(st: SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59)) {
        override def unsafeEncodeValue(st: SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait59[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait60[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A, A60 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]], M60: Meta[A60], A60: Lazy[Encoder[A60]]): Encoder[SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60]] = {
    def work(st: SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
      case SealedTrait._60(v) => A60.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60)) {
        override def unsafeEncodeValue(st: SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait60[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait61[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A, A60 <: A, A61 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]], M60: Meta[A60], A60: Lazy[Encoder[A60]], M61: Meta[A61], A61: Lazy[Encoder[A61]]): Encoder[SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61]] = {
    def work(st: SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
      case SealedTrait._60(v) => A60.value.unsafeEncode(v, indent, out)
      case SealedTrait._61(v) => A61.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61)) {
        override def unsafeEncodeValue(st: SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait61[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait62[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A, A60 <: A, A61 <: A, A62 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]], M60: Meta[A60], A60: Lazy[Encoder[A60]], M61: Meta[A61], A61: Lazy[Encoder[A61]], M62: Meta[A62], A62: Lazy[Encoder[A62]]): Encoder[SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62]] = {
    def work(st: SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
      case SealedTrait._60(v) => A60.value.unsafeEncode(v, indent, out)
      case SealedTrait._61(v) => A61.value.unsafeEncode(v, indent, out)
      case SealedTrait._62(v) => A62.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62)) {
        override def unsafeEncodeValue(st: SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait62[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait63[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A, A60 <: A, A61 <: A, A62 <: A, A63 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]], M60: Meta[A60], A60: Lazy[Encoder[A60]], M61: Meta[A61], A61: Lazy[Encoder[A61]], M62: Meta[A62], A62: Lazy[Encoder[A62]], M63: Meta[A63], A63: Lazy[Encoder[A63]]): Encoder[SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63]] = {
    def work(st: SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
      case SealedTrait._60(v) => A60.value.unsafeEncode(v, indent, out)
      case SealedTrait._61(v) => A61.value.unsafeEncode(v, indent, out)
      case SealedTrait._62(v) => A62.value.unsafeEncode(v, indent, out)
      case SealedTrait._63(v) => A63.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62, M63)) {
        override def unsafeEncodeValue(st: SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62, M63), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait63[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }

  implicit def sealedtrait64[A, A1 <: A, A2 <: A, A3 <: A, A4 <: A, A5 <: A, A6 <: A, A7 <: A, A8 <: A, A9 <: A, A10 <: A, A11 <: A, A12 <: A, A13 <: A, A14 <: A, A15 <: A, A16 <: A, A17 <: A, A18 <: A, A19 <: A, A20 <: A, A21 <: A, A22 <: A, A23 <: A, A24 <: A, A25 <: A, A26 <: A, A27 <: A, A28 <: A, A29 <: A, A30 <: A, A31 <: A, A32 <: A, A33 <: A, A34 <: A, A35 <: A, A36 <: A, A37 <: A, A38 <: A, A39 <: A, A40 <: A, A41 <: A, A42 <: A, A43 <: A, A44 <: A, A45 <: A, A46 <: A, A47 <: A, A48 <: A, A49 <: A, A50 <: A, A51 <: A, A52 <: A, A53 <: A, A54 <: A, A55 <: A, A56 <: A, A57 <: A, A58 <: A, A59 <: A, A60 <: A, A61 <: A, A62 <: A, A63 <: A, A64 <: A](implicit M: Meta[A], M1: Meta[A1], A1: Lazy[Encoder[A1]], M2: Meta[A2], A2: Lazy[Encoder[A2]], M3: Meta[A3], A3: Lazy[Encoder[A3]], M4: Meta[A4], A4: Lazy[Encoder[A4]], M5: Meta[A5], A5: Lazy[Encoder[A5]], M6: Meta[A6], A6: Lazy[Encoder[A6]], M7: Meta[A7], A7: Lazy[Encoder[A7]], M8: Meta[A8], A8: Lazy[Encoder[A8]], M9: Meta[A9], A9: Lazy[Encoder[A9]], M10: Meta[A10], A10: Lazy[Encoder[A10]], M11: Meta[A11], A11: Lazy[Encoder[A11]], M12: Meta[A12], A12: Lazy[Encoder[A12]], M13: Meta[A13], A13: Lazy[Encoder[A13]], M14: Meta[A14], A14: Lazy[Encoder[A14]], M15: Meta[A15], A15: Lazy[Encoder[A15]], M16: Meta[A16], A16: Lazy[Encoder[A16]], M17: Meta[A17], A17: Lazy[Encoder[A17]], M18: Meta[A18], A18: Lazy[Encoder[A18]], M19: Meta[A19], A19: Lazy[Encoder[A19]], M20: Meta[A20], A20: Lazy[Encoder[A20]], M21: Meta[A21], A21: Lazy[Encoder[A21]], M22: Meta[A22], A22: Lazy[Encoder[A22]], M23: Meta[A23], A23: Lazy[Encoder[A23]], M24: Meta[A24], A24: Lazy[Encoder[A24]], M25: Meta[A25], A25: Lazy[Encoder[A25]], M26: Meta[A26], A26: Lazy[Encoder[A26]], M27: Meta[A27], A27: Lazy[Encoder[A27]], M28: Meta[A28], A28: Lazy[Encoder[A28]], M29: Meta[A29], A29: Lazy[Encoder[A29]], M30: Meta[A30], A30: Lazy[Encoder[A30]], M31: Meta[A31], A31: Lazy[Encoder[A31]], M32: Meta[A32], A32: Lazy[Encoder[A32]], M33: Meta[A33], A33: Lazy[Encoder[A33]], M34: Meta[A34], A34: Lazy[Encoder[A34]], M35: Meta[A35], A35: Lazy[Encoder[A35]], M36: Meta[A36], A36: Lazy[Encoder[A36]], M37: Meta[A37], A37: Lazy[Encoder[A37]], M38: Meta[A38], A38: Lazy[Encoder[A38]], M39: Meta[A39], A39: Lazy[Encoder[A39]], M40: Meta[A40], A40: Lazy[Encoder[A40]], M41: Meta[A41], A41: Lazy[Encoder[A41]], M42: Meta[A42], A42: Lazy[Encoder[A42]], M43: Meta[A43], A43: Lazy[Encoder[A43]], M44: Meta[A44], A44: Lazy[Encoder[A44]], M45: Meta[A45], A45: Lazy[Encoder[A45]], M46: Meta[A46], A46: Lazy[Encoder[A46]], M47: Meta[A47], A47: Lazy[Encoder[A47]], M48: Meta[A48], A48: Lazy[Encoder[A48]], M49: Meta[A49], A49: Lazy[Encoder[A49]], M50: Meta[A50], A50: Lazy[Encoder[A50]], M51: Meta[A51], A51: Lazy[Encoder[A51]], M52: Meta[A52], A52: Lazy[Encoder[A52]], M53: Meta[A53], A53: Lazy[Encoder[A53]], M54: Meta[A54], A54: Lazy[Encoder[A54]], M55: Meta[A55], A55: Lazy[Encoder[A55]], M56: Meta[A56], A56: Lazy[Encoder[A56]], M57: Meta[A57], A57: Lazy[Encoder[A57]], M58: Meta[A58], A58: Lazy[Encoder[A58]], M59: Meta[A59], A59: Lazy[Encoder[A59]], M60: Meta[A60], A60: Lazy[Encoder[A60]], M61: Meta[A61], A61: Lazy[Encoder[A61]], M62: Meta[A62], A62: Lazy[Encoder[A62]], M63: Meta[A63], A63: Lazy[Encoder[A63]], M64: Meta[A64], A64: Lazy[Encoder[A64]]): Encoder[SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64]] = {
    def work(st: SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64], indent: Option[Int], out: java.io.Writer): Unit = st match {
      case SealedTrait._1(v) => A1.value.unsafeEncode(v, indent, out)
      case SealedTrait._2(v) => A2.value.unsafeEncode(v, indent, out)
      case SealedTrait._3(v) => A3.value.unsafeEncode(v, indent, out)
      case SealedTrait._4(v) => A4.value.unsafeEncode(v, indent, out)
      case SealedTrait._5(v) => A5.value.unsafeEncode(v, indent, out)
      case SealedTrait._6(v) => A6.value.unsafeEncode(v, indent, out)
      case SealedTrait._7(v) => A7.value.unsafeEncode(v, indent, out)
      case SealedTrait._8(v) => A8.value.unsafeEncode(v, indent, out)
      case SealedTrait._9(v) => A9.value.unsafeEncode(v, indent, out)
      case SealedTrait._10(v) => A10.value.unsafeEncode(v, indent, out)
      case SealedTrait._11(v) => A11.value.unsafeEncode(v, indent, out)
      case SealedTrait._12(v) => A12.value.unsafeEncode(v, indent, out)
      case SealedTrait._13(v) => A13.value.unsafeEncode(v, indent, out)
      case SealedTrait._14(v) => A14.value.unsafeEncode(v, indent, out)
      case SealedTrait._15(v) => A15.value.unsafeEncode(v, indent, out)
      case SealedTrait._16(v) => A16.value.unsafeEncode(v, indent, out)
      case SealedTrait._17(v) => A17.value.unsafeEncode(v, indent, out)
      case SealedTrait._18(v) => A18.value.unsafeEncode(v, indent, out)
      case SealedTrait._19(v) => A19.value.unsafeEncode(v, indent, out)
      case SealedTrait._20(v) => A20.value.unsafeEncode(v, indent, out)
      case SealedTrait._21(v) => A21.value.unsafeEncode(v, indent, out)
      case SealedTrait._22(v) => A22.value.unsafeEncode(v, indent, out)
      case SealedTrait._23(v) => A23.value.unsafeEncode(v, indent, out)
      case SealedTrait._24(v) => A24.value.unsafeEncode(v, indent, out)
      case SealedTrait._25(v) => A25.value.unsafeEncode(v, indent, out)
      case SealedTrait._26(v) => A26.value.unsafeEncode(v, indent, out)
      case SealedTrait._27(v) => A27.value.unsafeEncode(v, indent, out)
      case SealedTrait._28(v) => A28.value.unsafeEncode(v, indent, out)
      case SealedTrait._29(v) => A29.value.unsafeEncode(v, indent, out)
      case SealedTrait._30(v) => A30.value.unsafeEncode(v, indent, out)
      case SealedTrait._31(v) => A31.value.unsafeEncode(v, indent, out)
      case SealedTrait._32(v) => A32.value.unsafeEncode(v, indent, out)
      case SealedTrait._33(v) => A33.value.unsafeEncode(v, indent, out)
      case SealedTrait._34(v) => A34.value.unsafeEncode(v, indent, out)
      case SealedTrait._35(v) => A35.value.unsafeEncode(v, indent, out)
      case SealedTrait._36(v) => A36.value.unsafeEncode(v, indent, out)
      case SealedTrait._37(v) => A37.value.unsafeEncode(v, indent, out)
      case SealedTrait._38(v) => A38.value.unsafeEncode(v, indent, out)
      case SealedTrait._39(v) => A39.value.unsafeEncode(v, indent, out)
      case SealedTrait._40(v) => A40.value.unsafeEncode(v, indent, out)
      case SealedTrait._41(v) => A41.value.unsafeEncode(v, indent, out)
      case SealedTrait._42(v) => A42.value.unsafeEncode(v, indent, out)
      case SealedTrait._43(v) => A43.value.unsafeEncode(v, indent, out)
      case SealedTrait._44(v) => A44.value.unsafeEncode(v, indent, out)
      case SealedTrait._45(v) => A45.value.unsafeEncode(v, indent, out)
      case SealedTrait._46(v) => A46.value.unsafeEncode(v, indent, out)
      case SealedTrait._47(v) => A47.value.unsafeEncode(v, indent, out)
      case SealedTrait._48(v) => A48.value.unsafeEncode(v, indent, out)
      case SealedTrait._49(v) => A49.value.unsafeEncode(v, indent, out)
      case SealedTrait._50(v) => A50.value.unsafeEncode(v, indent, out)
      case SealedTrait._51(v) => A51.value.unsafeEncode(v, indent, out)
      case SealedTrait._52(v) => A52.value.unsafeEncode(v, indent, out)
      case SealedTrait._53(v) => A53.value.unsafeEncode(v, indent, out)
      case SealedTrait._54(v) => A54.value.unsafeEncode(v, indent, out)
      case SealedTrait._55(v) => A55.value.unsafeEncode(v, indent, out)
      case SealedTrait._56(v) => A56.value.unsafeEncode(v, indent, out)
      case SealedTrait._57(v) => A57.value.unsafeEncode(v, indent, out)
      case SealedTrait._58(v) => A58.value.unsafeEncode(v, indent, out)
      case SealedTrait._59(v) => A59.value.unsafeEncode(v, indent, out)
      case SealedTrait._60(v) => A60.value.unsafeEncode(v, indent, out)
      case SealedTrait._61(v) => A61.value.unsafeEncode(v, indent, out)
      case SealedTrait._62(v) => A62.value.unsafeEncode(v, indent, out)
      case SealedTrait._63(v) => A63.value.unsafeEncode(v, indent, out)
      case SealedTrait._64(v) => A64.value.unsafeEncode(v, indent, out)
    }
    Option("hintname") match {
      case None => new SealedTraitEncoder[A, SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62, M63, M64)) {
        override def unsafeEncodeValue(st: SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64]](Array(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49, M50, M51, M52, M53, M54, M55, M56, M57, M58, M59, M60, M61, M62, M63, M64), hintfield) {
        override def unsafeEncodeValue(st: SealedTrait64[A, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64], indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
      }
    }
  }
}
