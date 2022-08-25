package json.value.spec.parser

import com.github.plokhotnyuk.jsoniter_scala.core.{ReaderConfig, readFromArray, readFromString}
import json.value.{Codec, JsObj, Json}

private[json] trait JsonParser[T <: Json[T]] extends Parser[T]:

  def parse(json: String): T

  def parse(json: Array[Byte]): T

  def parse(json: String,config: ReaderConfig): T

  def parse(json: Array[Byte],config: ReaderConfig): T


