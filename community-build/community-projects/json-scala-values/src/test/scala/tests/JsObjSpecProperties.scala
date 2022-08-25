package tests

import java.time.Instant
import json.value.*
import json.value.gen.JsObjGen
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop
import json.value.gen.Conversions.given
import json.value.spec.*
import json.value.spec.parser.*
import scala.language.implicitConversions

object JsObjSpecProperties extends org.scalacheck.Properties("Json Object Spec") {

  val gen = JsObjGen(
    "a" -> Arbitrary.arbitrary[Int],
    "b" -> Arbitrary.arbitrary[Long],
    "c" -> Arbitrary.arbitrary[String],
    "d" -> Arbitrary.arbitrary[Boolean],
    "e" -> Arbitrary.arbitrary[BigInt],
    "f" -> Arbitrary.arbitrary[BigDecimal]
  )
  val spec: JsObjSpec =
    JsObjSpec(
      "a" -> IsInt,
      "b" -> IsLong,
      "c" -> IsStr,
      "d" -> IsBool,
      "e" -> IsIntegral,
      "f" -> IsNumber
    )

  val parser: JsObjSpecParser = spec.parser



  property("validating primitive specs") = Prop.forAll(gen)(o=>
    val errors =  spec.validateAll(o)
    errors.foreach(println)
    errors.isEmpty
  )

  def mapGen(valueGen:Gen[JsValue],keyGen:Gen[String]=Gen.alphaStr)=
    val pairGen:Gen[(String,JsValue)] =
      for
        key <- keyGen
        value <- valueGen
      yield (key,value)
    Gen.mapOf(pairGen).map(JsObj.apply)


  val gen1: Gen[JsObj] = JsObjGen(
    "a" -> mapGen(Arbitrary.arbitrary[Int]),
    "b" -> mapGen(Arbitrary.arbitrary[Long]),
    "c" -> mapGen(Arbitrary.arbitrary[BigInt]),
    "d" -> mapGen(Arbitrary.arbitrary[Boolean]),
    "f" -> mapGen(Arbitrary.arbitrary[BigDecimal]),
    "g" -> mapGen(Arbitrary.arbitrary[String]),
    "h" -> mapGen(Arbitrary.arbitrary[Instant])
  )

  val spec1 = JsObjSpec.apply("a" -> IsMapOfInt,
    "b" -> IsMapOfLong,
    "c" -> IsMapOfIntegral,
    "d" -> IsMapOfBool,
    "f" -> IsMapOfNumber,
    "g" -> IsMapOfStr,
    "h" -> IsMapOfInstant
  )

  property("validating map specs") = Prop.forAll(gen1)(o=> {
    val errors = spec1.validateAll(o)
    errors.foreach(println)
    errors.isEmpty
  }
  )



}



