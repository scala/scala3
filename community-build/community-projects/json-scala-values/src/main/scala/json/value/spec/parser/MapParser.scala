package json.value.spec.parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import json.value.spec.codec.JsObjCodec
import json.value.{JsObj, JsValue}


final class MapParser(private[json] val valueParser:Parser[_],p:JsValue=>Boolean|String,k:String=>Boolean|String)
  extends JsonSpecParser[JsObj]:

  private val mapCodec = JsObjCodec(this)

  override private[json] def parse(in: JsonReader) =
      val b = in.nextToken()
      if b == '{' then parseObjAfterOpenBrace(in,valueParser,p,k)
      else in.decodeError(ParserSpecError.START_OBJECT_EXPECTED)

  @inline override private[json] def codec = mapCodec



