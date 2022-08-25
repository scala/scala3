package tests
import scala.util.Try
import scala.util.Failure
import json.value.*
import json.value.Conversions.given
import json.value.spec.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import java.time.Instant
import scala.language.implicitConversions

class JsParserObjTests extends AnyFlatSpec with should.Matchers {

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



    def parser = spec.parser

    the [JsonReaderException] thrownBy {
      parser.parse(JsObj("a" -> JsInt(-1)).toPrettyString())
    } should have message "lower than zero, offset: 0x0000000c"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("a" -> JsObj.empty).toPrettyString())
    } should have message "illegal number, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("b" -> JsInt(-1)).toPrettyString())
    } should have message "lower than zero, offset: 0x0000000c"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("b" -> JsStr("5")).toPrettyString())
    } should have message "illegal number, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("c" -> JsStr("")).toPrettyString())
    } should have message "empty string, offset: 0x0000000c"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("c" -> JsInt(1)).toPrettyString())
    } should have message "expected '\"', offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("d" -> JsInstant(Instant.EPOCH)).toPrettyString())
    } should have message "before epoch, offset: 0x00000020"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("d" -> JsStr("")).toPrettyString())
    } should have message "unexpected end of input, offset: 0x0000000f"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("e" -> JsBool.TRUE).toPrettyString())
    } should have message "illegal number, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("e" -> JsBigDec(1.5)).toPrettyString())
    } should have message "not valid long, offset: 0x0000000d"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("f"-> JsBigInt(BigInt("1111111111111111111111111111111"))).toPrettyString())
    } should have message "not valid long, offset: 0x00000029"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("f" -> JsBool.TRUE).toPrettyString())
    } should have message "illegal number, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj( "g" -> JsInt(1111)).toPrettyString())
    } should have message "illegal boolean, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("h" -> JsObj.empty).toPrettyString())
    } should have message "empty, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("h" -> JsStr("a")).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("i" -> JsArray.empty).toPrettyString())
    } should have message "empty, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("i" -> JsInt(1)).toPrettyString())
    } should have message "start JSON array '[' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("j" -> JsObj("a" -> JsStr(""))).toPrettyString())
    } should have message "val empty, offset: 0x0000001b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("j" -> JsObj("" -> JsStr("a"))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("j" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "expected '\"', offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("j" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("k" -> JsObj("a" -> JsObj.empty)).toPrettyString())
    } should have message "val empty, offset: 0x00000031"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("k" -> JsObj("" -> JsObj("a" -> JsBool.TRUE))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("k" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("k" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("l" -> JsObj("a" -> JsArray.empty)).toPrettyString())
    } should have message "val empty, offset: 0x00000031"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("l" -> JsObj("" -> JsArray(1))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("l" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "start JSON array '[' expected, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("l" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"


    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("m" -> JsObj("a" -> JsInt(-1))).toPrettyString())
    } should have message "lower than zero, offset: 0x0000001b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("m" -> JsObj("" -> JsInt(1))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("m" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "illegal number, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("m" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"


    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("n" -> JsObj("a" -> JsInt(-1))).toPrettyString())
    } should have message "lower than zero, offset: 0x0000001b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("n" -> JsObj("" -> JsInt(1))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("n" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "illegal number, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("n" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"


    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("o" -> JsObj("a" -> JsInstant(Instant.now()))).toPrettyString())
    } should have message "after epoch, offset: 0x00000036"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("o" -> JsObj("" -> JsInstant(Instant.EPOCH))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("o" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "expected '\"', offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("o" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("p" -> JsObj("a" -> JsLong(1))).toPrettyString())
    } should have message "valid long, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("p" -> JsObj("" -> JsBigInt(BigInt("11111111111111111111111111111")))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("p" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "illegal number, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("p" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("q" -> JsObj("a" -> JsInt(1))).toPrettyString())
    } should have message "valid int, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("q" -> JsObj("" -> JsBigDec(BigDecimal(1.5)))).toPrettyString())
    } should have message "key empty, offset: 0x00000017"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("q" -> JsObj("a" -> JsBool.TRUE)).toPrettyString())
    } should have message "illegal number, offset: 0x0000001a"

    the[JsonReaderException] thrownBy {
      parser.parse(JsObj("q" -> JsBool.TRUE).toPrettyString())
    } should have message "start JSON object '{' expected, offset: 0x0000000b"
  }



}