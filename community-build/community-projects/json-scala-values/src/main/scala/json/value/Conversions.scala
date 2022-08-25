package json.value

import json.value.*
import java.time.Instant
import scala.Conversion
import scala.language.implicitConversions

object Conversions:
  given  Conversion[String, JsStr] = JsStr(_)

  given  Conversion[Int, JsInt] = JsInt(_)

  given  Conversion[Long, JsLong] = JsLong(_)

  given Conversion[BigInt, JsBigInt] = JsBigInt(_)

  given Conversion[Double, JsDouble] = JsDouble(_)

  given Conversion[BigDecimal, JsBigDec] = JsBigDec(_)

  given Conversion[Boolean, JsBool] = JsBool(_)

  given Conversion[Instant, JsInstant] = JsInstant(_)