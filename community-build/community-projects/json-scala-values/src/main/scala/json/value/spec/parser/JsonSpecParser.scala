package json.value.spec.parser
import com.github.plokhotnyuk.jsoniter_scala.core.{ReaderConfig, readFromArray, readFromString}
import json.value.Json
import json.value.{Codec, JsObj}

private[json] trait JsonSpecParser[T <: Json[T]] extends JsonParser[T] {

  @inline private[json] def codec:Codec[T]

  override def parse(json: String): T = readFromString(json,ParserConf.DEFAULT_READER_CONFIG)(codec)

  override def parse(json: Array[Byte]): T = readFromArray(json,ParserConf.DEFAULT_READER_CONFIG)(codec)

  override def parse(json: String,config: ReaderConfig): T = readFromString(json,config)(codec)

  override def parse(json: Array[Byte],config: ReaderConfig): T = readFromArray(json,config)(codec)


}
