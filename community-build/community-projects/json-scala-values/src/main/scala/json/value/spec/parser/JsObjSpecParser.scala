package json.value.spec.parser
import scala.collection.mutable.LinkedHashMap
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter, readFromArray, readFromString}
import json.value.spec.Invalid
import json.value.spec.codec.JsObjCodec
import json.value.spec.parser.Parser
import json.value.{JsObj, JsPath, JsValue}

import scala.annotation.tailrec

final case class JsObjSpecParser(private[json] val parsers: Map[String,Parser[_]],
                                 private[json] val strict:Boolean,
                                 private[json] val required:Seq[String],
                                 private[parser] val lenientParser:Parser[JsValue] = JsValueParser.DEFAULT)
  extends JsonSpecParser[JsObj]:

  private val objCodec = JsObjCodec(this)

  @tailrec
  private def validateRequired(x:Map[String,JsValue],keys:Seq[String],in: JsonReader):Unit =
    if keys.nonEmpty then
      if x.contains(keys.head) then validateRequired(x,keys.tail,in)
      else in.decodeError(ParserSpecError.KEY_REQUIRED)
  private[json] override def parse(in: JsonReader): JsObj =
    val b = in.nextToken()
    if b == '{' then
      if in.isNextToken('}') then return JsObj.empty
      in.rollbackToken()
      var map:Map[String,JsValue] = Map.empty
      var isNextToken = true
      while isNextToken do
        val key = in.readKeyAsString()
        parsers.get(key) match
          case Some(parser) => map = map.updated(key, parser.parse(in))
          case None =>
            if strict then in.decodeError(ParserSpecError.SPEC_FOR_VALUE_NOT_DEFINED)
            map = map.updated(key, lenientParser.parse(in))
        isNextToken = in.isNextToken(',')
      if in.isCurrentToken('}')
      then
        if required.isEmpty then return new JsObj(map)
        else
          validateRequired(map,required,in)
          return new JsObj(map)
      in.objectEndOrCommaError()
    in.decodeError(ParserSpecError.START_OBJECT_EXPECTED)

  @inline override private[json] def codec = objCodec


