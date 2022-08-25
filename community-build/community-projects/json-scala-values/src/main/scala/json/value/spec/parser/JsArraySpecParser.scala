package json.value.spec.parser

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter, readFromArray, readFromString}
import json.value.spec.codec.JsArrayCodec
import json.value.spec.parser.Parser
import json.value.{JsArray, JsObj, JsValue}

final case class JsArraySpecParser(private[json] val parsers: Seq[Parser[_]], strict: Boolean)
  extends JsonSpecParser[JsArray]:

  private val arrayCodec =  JsArrayCodec(this)
  override def parse(in: JsonReader): JsArray =
    if in.isNextToken('[') then
      if in.isNextToken(']') then return JsArray.empty
      in.rollbackToken()
      var list= List.empty[JsValue]
      var i = 0
      var isNextToken = true
      val maxIndex = parsers.size - 1
      while isNextToken do
        list = list.appended(parsers(i).parse(in))
        if strict && i > maxIndex then in.decodeError(ParserSpecError.SPEC_FOR_VALUE_NOT_DEFINED)
        i += 1
        isNextToken = in.isNextToken(',')
      if in.isCurrentToken(']') then return JsArray(list)
      in.arrayEndOrCommaError()
    in.decodeError(ParserSpecError.START_ARRAY_EXPECTED)

  @inline override private[json] def codec = arrayCodec

