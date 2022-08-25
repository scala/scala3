package json.value.spec.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}
import json.value.{Codec, JsArray}
import json.value.spec.parser.{JsArraySpecParser, Parser}

private[value] case class JsArrayCodec(parser:Parser[JsArray]) extends Codec[JsArray]:
 override def decodeValue(in: JsonReader, 
                          default: JsArray): JsArray = parser.parse(in)

 override def encodeValue(x: JsArray, 
                          out: JsonWriter): Unit = super.encodeJsArray(x, out)

 override def nullValue: JsArray = JsArray.empty


