package tests

import json.value.{JsBigDec, JsBigInt, JsDouble, JsInt, JsLong, JsValue}
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  val longGen:Gen[JsValue] =
    Gen.oneOf(Arbitrary.arbitrary[Long].map(JsLong(_)),
              Arbitrary.arbitrary[Int].map(JsInt(_)))

  val doubleGen:Gen[JsValue] =
    Gen.oneOf(Arbitrary.arbitrary[Long].map(JsLong(_)),
              Arbitrary.arbitrary[Int].map(JsInt(_)),
              Arbitrary.arbitrary[Double].map(JsDouble(_)))

  val bigIntGen:Gen[JsValue] =
    Gen.oneOf(Arbitrary.arbitrary[Long].map(JsLong(_)),
              Arbitrary.arbitrary[Int].map(JsInt(_)),
              Arbitrary.arbitrary[BigInt].map(JsBigInt(_)))


  val bigDecGen:Gen[JsValue] =
    Gen.oneOf(Arbitrary.arbitrary[BigDecimal].map(JsBigDec(_)),
              Arbitrary.arbitrary[Long].map(JsLong(_)),
              Arbitrary.arbitrary[Int].map(JsInt(_)),
              Arbitrary.arbitrary[BigInt].map(JsBigInt(_)))
  
}
