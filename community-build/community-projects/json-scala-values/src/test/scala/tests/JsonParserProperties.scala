package tests
import json.value.spec.*
import json.value.*
import json.value.gen.Conversions.given
import json.value.gen.{JsArrayGen, JsObjGen, *}
import org.scalacheck.Gen.Parameters
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Test}

import java.time.Instant
import scala.language.implicitConversions
class JsonParserProperties extends org.scalacheck.Properties("Json Parser") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(10000)
     .withWorkers(8)

  private val g: Gen[Double] = Gen.choose(0.5, 1.5)
  

  private val gen =
      JsObjGen("a" -> Gen.oneOf(Gen.alphaStr.map(JsStr(_)),Arbitrary.arbitrary[Long].map(JsLong(_))),
               "b" -> Gen.oneOf(Arbitrary.arbitrary[Int].map(JsInt(_)),Arbitrary.arbitrary[Boolean].map(JsBool(_))),
               "c" -> Generators.longGen,
               "d" -> Generators.doubleGen,
               "e" -> Generators.bigIntGen,
               "f" -> Generators.bigDecGen,
               "g" -> Arbitrary.arbitrary[Boolean],
               "h" -> Arbitrary.arbitrary[Instant],
               "i" -> Gen.choose(1,1000).map(JsInt.apply),
               "j" -> Gen.choose(1L,1000L).map(JsLong.apply),
               "k" -> Gen.asciiStr.suchThat(_.nonEmpty),
               "arr" -> TupleGen(
                 JsArrayGen.of(Arbitrary.arbitrary[Int].retryUntil(a=> a % 2 == 0)),
                 JsArrayGen.of(Arbitrary.arbitrary[Long].retryUntil(a=> a % 2 == 1)),
                 JsArrayGen.noneEmptyOf(g),
                 JsArrayGen.of(Arbitrary.arbitrary[BigInt]),
                 JsArrayGen.of(Arbitrary.arbitrary[Instant]),
                 JsArrayGen.of(Arbitrary.arbitrary[BigDecimal]),
                 JsArrayGen.of(Arbitrary.arbitrary[Boolean]),
                 JsArrayGen.of(Gen.const(JsNull)),
                 JsArrayGen.of(JsObjGen("a" -> Arbitrary.arbitrary[Boolean],
                                        "b" -> Arbitrary.arbitrary[Int])),
                 JsArrayGen.of(Gen.oneOf(JsObjGen("a" -> Gen.alphaStr, "b" -> Arbitrary.arbitrary[Int]),
                                         JsObjGen("c" -> Arbitrary.arbitrary[Int], "d" -> Gen.alphaStr)))
               ),
               "i" -> JsObjGen("a" -> JsArrayGen.ofN(3,Gen.asciiStr),
                               "b" -> TupleGen(Gen.const(JsObj.empty),TupleGen(Gen.const(JsObj.empty),Gen.const(JsNull))))
      )




  private val spec = JsObjSpec(
    "a" -> IsStr.or(IsLong),
    "b" -> IsInt.or(IsBool),
    "c" -> IsLong,
    "d" -> IsNumber,
    "e" -> IsIntegral,
    "f" -> IsNumber,
    "g" -> IsBool,
    "h" -> IsInstant,
    "i" -> IsInt(n => if n > 0 then true else "lower than zero"),
    "j" -> IsLong(n => if n > 0 then true else "lower than zero"),
    "k" -> IsStr(s => if s.nonEmpty then true else "empty string"),
    "arr" -> IsTuple(
      IsArrayOf(IsInt(_ % 2 == 0)),
      IsArrayOf(IsLong(_ % 2 == 1)),
      IsArrayOf(IsNumber(n => n >= 0.5d || n <= 1.5d)),
      IsArrayOf(IsIntegral),
      IsArrayOf(IsInstant),
      IsArrayOf(IsNumber),
      IsArrayOf(IsBool),
      IsArrayOf(IsNull),
      IsArrayOf(JsObjSpec("a" -> IsBool, "b" -> IsInt)),
      IsArrayOf(JsObjSpec("a" -> IsStr, "b" -> IsInt).or(JsObjSpec("c" -> IsInt, "d" -> IsStr)))),
    "i" -> JsObjSpec("a" -> IsArray,"b" -> IsTuple(IsJsObj,IsTuple(IsJsObj,IsNull)))
  )

  val parser = spec.parser

  property("obj -> toString -> parse from spec returns the same json") = forAll(gen) { obj =>
    parser.parse(obj.toString) == obj
  }

  property("obj -> toString -> parse from spec returns the same json with the same hashcode") = forAll(gen) { obj =>
    parser.parse(obj.toString).hashCode == obj.hashCode
  }

  property("obj -> toString -> parse returns the same json") = forAll(gen) { obj =>
    JsObj.parse(obj.toString) == obj
  }

  property("obj -> toPrettyString -> parse returns the same json") = forAll(gen) { obj =>
    JsObj.parse(obj.toPrettyString()) == obj
  }

  property("obj -> serialize -> parse returns the same json") = forAll(gen) { obj =>
    JsObj.parse(obj.serialize()) == obj
  }

  property("obj -> toString -> parse the same json with the same hashcode") = forAll(gen) { obj =>
    JsObj.parse(obj.toString).hashCode == obj.hashCode
  }

}
