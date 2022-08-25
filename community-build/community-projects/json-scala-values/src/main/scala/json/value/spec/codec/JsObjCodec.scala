package json.value.spec.codec

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}
import json.value.{Codec, JsObj}
import json.value.spec.parser.*
private[json] case class JsObjCodec(parser:Parser[JsObj]) extends Codec[JsObj]:
 override def decodeValue(in: JsonReader, 
                          default: JsObj): JsObj = parser.parse(in)
 override def encodeValue(x: JsObj, 
                          out: JsonWriter): Unit = super.encodeJsObj(x, out)
 override def nullValue: JsObj = JsObj.empty



