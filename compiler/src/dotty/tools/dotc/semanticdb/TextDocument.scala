package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._
import scala.annotation.internal.sharable

object TextDocument {
  val defaultInstance: TextDocument = TextDocument(Schema.LEGACY, Language.UNKNOWN_LANGUAGE, "", "", "", Nil, Nil)
}

final case class TextDocument(
  schema: Schema,
  language: Language,
  uri: String,
  text: String,
  md5: String,
  symbols: Seq[SymbolInformation],
  occurrences: Seq[SymbolOccurrence]
) extends SemanticdbMessage[TextDocument] derives CanEqual {
  @sharable
  private var __serializedSizeCachedValue: Int = 0
  private def __computeSerializedValue(): Int = {
    var __size = 0

    {
      val __value = schema
      if (__value != Schema.LEGACY) {
        __size += SemanticdbOutputStream.computeEnumSize(1, __value.value)
      }
    };

    {
      val __value = uri
      if (__value != "") {
        __size += SemanticdbOutputStream
          .computeStringSize(2, __value)
      }
    };

    {
      val __value = md5
      if (__value != "") {
        __size += SemanticdbOutputStream.computeStringSize(11, __value)
      }
    };

    {
      val __value = language
      if (__value != Language.UNKNOWN_LANGUAGE) {
        __size += SemanticdbOutputStream.computeEnumSize(10, __value.value)
      }
    };
    symbols.foreach { __item =>
      val __value = __item
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
    }
    occurrences.foreach { __item =>
      val __value = __item
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
        __value.serializedSize
      ) + __value.serializedSize
    }
    __size
  }
  final override def serializedSize: Int = {
    var read = __serializedSizeCachedValue
    if (read == 0) {
      read = __computeSerializedValue()
      __serializedSizeCachedValue = read
    }
    read
  }
  def writeTo(`_output__`: SemanticdbOutputStream): Unit = {
    {
      val __v = schema
      if (__v != Schema.LEGACY) {
        _output__.writeEnum(1, __v.value)
      }
    };
    {
      val __v = uri
      if (__v != "") {
        _output__.writeString(2, __v)
      }
    };
    symbols.foreach { __v =>
      val __m = __v
      _output__.writeTag(5, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
    occurrences.foreach { __v =>
      val __m = __v
      _output__.writeTag(6, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
    {
      val __v = language
      if (__v != Language.UNKNOWN_LANGUAGE) {
        _output__.writeEnum(10, __v.value)
      }
    };
    {
      val __v = md5
      if (__v != "") {
        _output__.writeString(11, __v)
      }
    };
  }
  def mergeFrom(`_input__`: SemanticdbInputStream): TextDocument = {
    var __schema = this.schema
    var __uri = this.uri
    var __md5 = this.md5
    var __language = this.language
    val __symbols = (Vector.newBuilder[SymbolInformation] ++= this.symbols)
    val __occurrences = (Vector.newBuilder[SymbolOccurrence] ++= this.occurrences)
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 8 =>
          __schema = Schema.fromValue(_input__.readEnum())
        case 18 =>
          __uri = _input__.readString()
        case 90 =>
          __md5 = _input__.readString()
        case 80 =>
          __language = Language.fromValue(_input__.readEnum())
        case 42 =>
          __symbols += LiteParser.readMessage(_input__, SymbolInformation.defaultInstance)
        case 50 =>
          __occurrences += LiteParser.readMessage(_input__, SymbolOccurrence.defaultInstance)
        case tag => _input__.skipField(tag)
      }
    }
    TextDocument(
      schema = __schema,
      uri = __uri,
      text = "",
      md5 = __md5,
      language = __language,
      symbols = __symbols.result(),
      occurrences = __occurrences.result(),
    )
  }
}
