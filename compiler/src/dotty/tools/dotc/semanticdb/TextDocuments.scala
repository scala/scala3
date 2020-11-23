package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._
import scala.annotation.internal.sharable

object TextDocuments {
  def parseFrom(in: Array[Byte]): TextDocuments = {
    parseFrom(SemanticdbInputStream.newInstance(in))
  }
  def parseFrom(in: SemanticdbInputStream): TextDocuments = {
    defaultInstance.mergeFrom(in)
  }
  val defaultInstance: TextDocuments = TextDocuments(Nil)
}
final case class TextDocuments(documents: Seq[TextDocument]) extends SemanticdbMessage[TextDocuments] derives CanEqual {
  @sharable
  private var __serializedSizeCachedValue: Int = 0
  private def __computeSerializedValue(): Int = {
    var __size = 0
    documents.foreach { __item =>
      val __value = __item
      __size += 1 +
        SemanticdbOutputStream.computeUInt32SizeNoTag(__value.serializedSize) +
        __value.serializedSize
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
    documents.foreach { __v =>
      val __m = __v
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
  }
  def mergeFrom(`_input__`: SemanticdbInputStream): TextDocuments = {
    val __documents = (Vector.newBuilder[TextDocument] ++= this.documents)
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
    TextDocuments(documents = __documents.result())
  }
}
