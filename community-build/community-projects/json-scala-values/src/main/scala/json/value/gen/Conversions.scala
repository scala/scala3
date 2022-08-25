package json.value.gen
import scala.language.implicitConversions
import scala.Conversion
import json.value.*
import org.scalacheck.Gen

import java.time.Instant

object Conversions:
  given strToJsStrGen: Conversion[Gen[String], Gen[JsStr]] = _.map(JsStr.apply)
  given intToJsIntGen: Conversion[Gen[Int], Gen[JsInt]] = _.map(JsInt.apply)
  given longToJsLongGen: Conversion[Gen[Long], Gen[JsLong]] = _.map(JsLong.apply)
  given bigIntToJsBigIntGen: Conversion[Gen[BigInt], Gen[JsBigInt]] = _.map(JsBigInt.apply)
  given doubleToJsDoubleGen: Conversion[Gen[Double], Gen[JsDouble]] = _.map(JsDouble.apply)
  given bigDecToJsBigDecGen: Conversion[Gen[BigDecimal], Gen[JsBigDec]] = _.map(JsBigDec.apply)
  given boolToJsBoolGen: Conversion[Gen[Boolean], Gen[JsBool]] = _.map(JsBool.apply)
  given instantToJsInstantGen: Conversion[Gen[Instant], Gen[JsInstant]] = _.map(JsInstant.apply)
