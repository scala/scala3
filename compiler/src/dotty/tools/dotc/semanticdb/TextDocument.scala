package dotty.tools.dotc.semanticdb

case class TextDocument(
    schema: Schema,
    language: Language,
    uri: String,
    md5: String,
    occurrences: Seq[SymbolOccurrence]
) extends SemanticdbMessage {
  private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
  private[this] def __computeSerializedValue(): _root_.scala.Int = {
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
    occurrences.foreach { __item =>
      val __value = __item
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
        __value.serializedSize
      ) + __value.serializedSize
    }
    __size
  }
  final override def serializedSize: _root_.scala.Int = {
    var read = __serializedSizeCachedValue
    if (read == 0) {
      read = __computeSerializedValue()
      __serializedSizeCachedValue = read
    }
    read
  }
  def writeTo(`_output__`: SemanticdbOutputStream): _root_.scala.Unit = {
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
}
