package json.value

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, JsonWriter}

import java.math.MathContext

private[value] trait Codec[T <: JsValue] extends JsonValueCodec[T]:

  def encodeValue(x: JsValue,
                  out: JsonWriter): Unit =
    x match
      case JsInt(n) => out.writeVal(n)
      case JsLong(n) => out.writeVal(n)
      case JsBigInt(n) => out.writeVal(n)
      case JsInstant(n) => out.writeVal(n)
      case JsBigDec(n) => out.writeVal(n)
      case JsDouble(n) => out.writeVal(BigDecimal(n))
      case JsStr(n) => out.writeVal(n)
      case JsBool(n) => out.writeVal(n)
      case JsNull => out.writeNull()
      case o:JsObj => encodeJsObj(o,out)
      case a:JsArray => encodeJsArray(a,out)
      case JsNothing => throw UnsupportedOperationException("JsNothing not serializable")

  def encodeJsArray(x: JsArray,
                    out: JsonWriter): Unit =
    out.writeArrayStart()
    x.iterator.foreach(v => encodeValue(v, out))
    out.writeArrayEnd()

  def encodeJsObj(x: JsObj,
                  out: JsonWriter):Unit =
    out.writeObjectStart()
    val it = x.bindings.iterator
    while it.hasNext do
      val (k, v) = it.next()
      out.writeKey(k)
      encodeValue(v, out)
    out.writeObjectEnd()
