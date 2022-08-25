package tests

import json.value.*
import json.value.gen.Conversions.given
import json.value.gen.JsObjGen
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import json.value.gen.*

import scala.annotation.nowarn
import scala.language.implicitConversions

//scalacheck uses Stream which is deprecated in Scala3
@nowarn
object JsObjGenProperties extends org.scalacheck.Properties("Json Object Generators") {

  val a = JsObjGen("a" -> Arbitrary.arbitrary[String], "b" -> Arbitrary.arbitrary[Int])
  val b = JsObjGen("c" -> Arbitrary.arbitrary[Boolean], "d" -> Arbitrary.arbitrary[Long])


  property("concat joins two generators") = forAll(a concat b)(_.size == 4)
  property("updated adds a new generator") = forAll(a updated ("c", Gen.alphaStr))(_.size == 3)


}

