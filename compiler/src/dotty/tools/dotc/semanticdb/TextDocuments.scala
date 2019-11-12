package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

object TextDocuments {
  def parseFrom(in: Array[Byte]): TextDocuments = {
    parseFrom(SemanticdbInputStream.newInstance(in))
  }
  def parseFrom(in: SemanticdbInputStream): TextDocuments = {
    defaultInstance.mergeFrom(in)
  }
  def defaultInstance: TextDocuments = TextDocuments(Nil)
}
case class TextDocuments(documents: Seq[TextDocument]) extends SemanticdbMessage[TextDocuments] {
  private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
  private[this] def __computeSerializedValue(): _root_.scala.Int = {
    var __size = 0
    documents.foreach { __item =>
      val __value = __item
      __size += 1 +
        SemanticdbOutputStream.computeUInt32SizeNoTag(__value.serializedSize) +
        __value.serializedSize
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
    documents.foreach { __v =>
      val __m = __v
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
  }
  def mergeFrom(`_input__`: SemanticdbInputStream): TextDocuments = {
    val __documents = (_root_.scala.collection.immutable.Vector.newBuilder[TextDocument] ++= this.documents)
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 10 =>
          __documents += LiteParser.readMessage(_input__, TextDocument.defaultInstance)
        case tag => _input__.skipField(tag)
      }
    }
    TextDocuments(
      documents = __documents.result()
    )
  }
}
