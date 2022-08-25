package tests
import json.value.spec.*
import json.value.*
import json.value.spec.SpecError.*
import json.value.Conversions.given
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import java.time.Instant
import scala.language.implicitConversions
class JsObjSpecTests extends AnyFlatSpec with should.Matchers {
  val mapSpecs = JsObjSpec(
    "a1" -> IsMapOfStr(keySuchThat= (k:String) => k.nonEmpty),
    "a2" -> IsMapOfObj(keySuchThat= (k:String) => k.nonEmpty),
    "a3" -> IsMapOfArr(keySuchThat= (k:String) => k.nonEmpty),
    "a4" -> IsMapOfInt(keySuchThat= (k:String) => k.nonEmpty),
    "a5" -> IsMapOfLong(keySuchThat= (k:String) => k.nonEmpty),
    "a6" -> IsMapOfInstant(keySuchThat= (k:String) => k.nonEmpty),
    "a7" -> IsMapOfIntegral(keySuchThat= (k:String) => k.nonEmpty),
    "a8" -> IsMapOfNumber(keySuchThat= (k:String) => k.nonEmpty),
    "a9" -> IsMapOfBool(keySuchThat= (k:String) => k.nonEmpty)
  )

  val mapCustomMessageSpecs = JsObjSpec(
    "j" -> IsMapOfStr(v => if v.isEmpty then "val empty" else true, k => if k.isEmpty then "key empty" else true),
    "k" -> IsMapOfObj(v => if v.isEmpty then "val empty" else true, k => if k.isEmpty then "key empty" else true),
    "l" -> IsMapOfArr(v => if v.isEmpty then "val empty" else true, k => if k.isEmpty then "key empty" else true),
    "m" -> IsMapOfInt(v => if v < 0 then "lower than zero" else true, k => if k.isEmpty then "key empty" else true),
    "n" -> IsMapOfLong(v => if v < 0 then "lower than zero" else true, k => if k.isEmpty then "key empty" else true),
    "o" -> IsMapOfInstant(v => if v.isAfter(Instant.EPOCH) then "after epoch" else true, k => if k.isEmpty then "key empty" else true),
    "p" -> IsMapOfIntegral(v => if v.isValidLong then "valid long" else true, k => if k.isEmpty then "key empty" else true),
    "q" -> IsMapOfNumber(v => if v.isValidInt then "valid int" else true, k => if k.isEmpty then "key empty" else true)
  )

  val mapDefaultMessageSpecs = JsObjSpec(
    "r" -> IsMapOfStr(v => v.nonEmpty , k => k.nonEmpty),
    "s" -> IsMapOfObj(v => v.nonEmpty, k => k.nonEmpty),
    "t" -> IsMapOfArr(v => v.nonEmpty, k => k.nonEmpty),
    "u" -> IsMapOfInt(v => if v < 0 then false else true, k => k.nonEmpty),
    "v" -> IsMapOfLong(v => if v < 0 then false else true, k =>k.nonEmpty),
    "w" -> IsMapOfInstant(v => if v.isAfter(Instant.EPOCH) then false else true, k => k.nonEmpty),
    "x" -> IsMapOfIntegral(v => if v.isValidLong then false else true, k => k.nonEmpty),
    "y" -> IsMapOfNumber(v => if v.isValidInt then false else true, k => k.nonEmpty)
  )

  "validateAll" should "return all the errors" in {

    val spec = JsObjSpec(
      "a" -> IsInt(n => if n > 0 then true else "lower than zero"),
      "b" -> IsLong(n => if n > 0 then true else "lower than zero"),
      "c" -> IsStr(s => if s.nonEmpty then true else "empty string"),
      "d" -> IsInstant(s => if s.isAfter(Instant.EPOCH) then true else "before epoch"),
      "e" -> IsNumber(s => if s.isValidLong then true else "not valid long"),
      "f" -> IsIntegral(s => if s.isValidLong then true else "not valid long"),
      "g" -> IsBool,
      "h" -> IsJsObj(s => if s.isEmpty then "empty" else true),
      "i" -> IsArray(s => if s.isEmpty then "empty" else true),
    ).concat(mapCustomMessageSpecs)
     .concat(mapDefaultMessageSpecs)
     .concat(mapSpecs)


    val expected = LazyList(
      (JsPath.root / "a", Invalid(-1, SpecError("lower than zero"))),
      (JsPath.root / "b", Invalid(-1, SpecError("lower than zero"))),
      (JsPath.root / "c", Invalid("", SpecError("empty string"))),
      (JsPath.root / "d", Invalid(Instant.EPOCH, SpecError("before epoch"))),
      (JsPath.root / "e", Invalid(BigDecimal(1.5), SpecError("not valid long"))),
      (JsPath.root / "f", Invalid(BigInt("1111111111111111111111111111111111111111111111111"), SpecError("not valid long"))),
      (JsPath.root / "g", Invalid("a", SpecError.BOOLEAN_EXPECTED)),
      (JsPath.root / "h", Invalid(JsObj.empty, SpecError("empty"))),
      (JsPath.root / "i", Invalid(JsArray.empty, SpecError("empty"))),
      (JsPath.root / "j" / "",Invalid("",SpecError("val empty"))),
      (JsPath.root / "j" / "",Invalid("",SpecError("key empty"))),
      (JsPath.root / "k" / "", Invalid(JsObj.empty, SpecError("val empty"))),
      (JsPath.root / "k" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "l" / "", Invalid(JsArray.empty, SpecError("val empty"))),
      (JsPath.root / "l" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "m" / "", Invalid(-1, SpecError("lower than zero"))),
      (JsPath.root / "m" / "", Invalid("", SpecError("key empty"))) ,
      (JsPath.root / "n" / "", Invalid(-1, SpecError("lower than zero"))),
      (JsPath.root / "n" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "o" / "", Invalid(Instant.MAX, SpecError("after epoch"))),
      (JsPath.root / "o" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "p" / "", Invalid(Long.MaxValue, SpecError("valid long"))),
      (JsPath.root / "p" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "q" / "", Invalid(Int.MaxValue, SpecError("valid int"))),
      (JsPath.root / "q" / "", Invalid("", SpecError("key empty"))),
      (JsPath.root / "r" / "", Invalid("", SpecError.STRING_CONDITION_FAILED)),
      (JsPath.root / "r" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "s" / "", Invalid(JsObj.empty, SpecError.OBJ_CONDITION_FAILED)),
      (JsPath.root / "s" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "t" / "", Invalid(JsArray.empty, SpecError.ARRAY_CONDITION_FAILED)),
      (JsPath.root / "t" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "u" / "", Invalid(-1, SpecError.INT_CONDITION_FAILED)),
      (JsPath.root / "u" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "v" / "", Invalid(-1, SpecError.LONG_CONDITION_FAILED)),
      (JsPath.root / "v" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "w" / "", Invalid(Instant.MAX, SpecError.INSTANT_CONDITION_FAILED)),
      (JsPath.root / "w" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "x" / "", Invalid(Long.MaxValue, SpecError.BIG_INTEGER_CONDITION_FAILED)),
      (JsPath.root / "x" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "y" / "", Invalid(Int.MaxValue, SpecError.DECIMAL_CONDITION_FAILED)),
      (JsPath.root / "y" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a1" / "", Invalid(0, SpecError.STRING_EXPECTED)),
      (JsPath.root / "a1" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a2" / "", Invalid(0, SpecError.OBJ_EXPECTED)),
      (JsPath.root / "a2" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a3" / "", Invalid(0, SpecError.ARRAY_EXPECTED)),
      (JsPath.root / "a3" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a4" / "", Invalid("", SpecError.INT_EXPECTED)),
      (JsPath.root / "a4" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a5" / "", Invalid("", SpecError.LONG_EXPECTED)),
      (JsPath.root / "a5" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a6" / "", Invalid(0, SpecError.INSTANT_EXPECTED)),
      (JsPath.root / "a6" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a7" / "", Invalid("", SpecError.BIG_INTEGER_EXPECTED)),
      (JsPath.root / "a7" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a8" / "", Invalid("", SpecError.DECIMAL_EXPECTED)),
      (JsPath.root / "a8" / "", Invalid("", SpecError.KEY_CONDITION_FAILED)),
      (JsPath.root / "a9" / "", Invalid("", SpecError.BOOLEAN_EXPECTED)),
      (JsPath.root / "a9" / "", Invalid("", SpecError.KEY_CONDITION_FAILED))
    )



    spec.validateAll(
      JsObj("a" -> -1,
            "b" -> -1,
            "c" -> "",
            "d" -> Instant.EPOCH,
            "e" -> BigDecimal(1.5),
            "f" -> BigInt("1111111111111111111111111111111111111111111111111"),
            "g" -> "a",
            "h" -> JsObj.empty,
            "i" -> JsArray.empty,
            "j" -> JsObj("" -> JsStr("")),
            "k" -> JsObj("" -> JsObj.empty),
            "l" -> JsObj("" -> JsArray.empty),
            "m" -> JsObj("" -> JsInt(-1)),
            "n" -> JsObj("" -> JsInt(-1)),
            "o" -> JsObj("" -> JsInstant(Instant.MAX)),
            "p" -> JsObj("" -> JsLong(Long.MaxValue)),
            "q" -> JsObj("" -> JsInt(Int.MaxValue)),
            "r" -> JsObj("" -> JsStr("")),
            "s" -> JsObj("" -> JsObj.empty),
            "t" -> JsObj("" -> JsArray.empty),
            "u" -> JsObj("" -> JsInt(-1)),
            "v" -> JsObj("" -> JsInt(-1)),
            "w" -> JsObj("" -> JsInstant(Instant.MAX)),
            "x" -> JsObj("" -> JsLong(Long.MaxValue)),
            "y" -> JsObj("" -> JsInt(Int.MaxValue)),
            "a1" -> JsObj("" -> JsInt(0)),
            "a2" -> JsObj("" -> JsInt(0)),
            "a3" -> JsObj("" -> JsInt(0)),
            "a4" -> JsObj("" -> JsStr("")),
            "a5" -> JsObj("" -> JsStr("")),
            "a6" -> JsObj("" -> JsInt(0)),
            "a7" -> JsObj("" -> JsStr("")),
            "a8" -> JsObj("" -> JsStr("")),
            "a9" -> JsObj("" -> JsStr(""))
           )) should be(expected)

  }


  "lenient operator" should "return a new spec" in {

    val spec = JsObjSpec("a" -> IsInstant)

    spec.validateAll(JsObj("a" -> JsStr(Instant.now().toString))) should be(LazyList.empty)

    spec.validateAll(JsObj("a" -> JsStr(Instant.now().toString),
                           "b" -> JsInt(1))) should be(LazyList((JsPath.root / "b",Invalid(JsNothing,SpecError.SPEC_FOR_VALUE_NOT_DEFINED))))

    spec.lenient.validateAll(JsObj("a" -> JsStr(Instant.now().toString), "b" -> JsInt(1))) should be(LazyList.empty)


  }

  "cons and enum" should "be used to validate constants" in {

    val spec  = JsObjSpec("a" -> IsCons(1),
                          "b" -> IsEnum("orange","apple"))

    val obj:JsObj = JsObj("a" -> 2, "b" -> "melon")
    spec.validateAll(obj) should
      be(LazyList((JsPath.root / "a",Invalid(2,SpecError.CONS_EXPECTED)),
        (JsPath.root / "b",Invalid("melon",SpecError.ENUM_VAL_EXPECTED))))

    val a:JsObj = JsObj("a" -> 1, "b" -> "orange")
    val b:JsObj = JsObj("a" -> 1, "b" -> "apple")

    spec.parser.parse(a.serialize()) should be(a)
    spec.parser.parse(b.serialize()) should be(b)

  }


}