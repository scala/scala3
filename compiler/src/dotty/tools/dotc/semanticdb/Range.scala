package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

object Range {
  def defaultInstance: Range = Range(0, 0, 0, 0)
}
case class Range(
    startLine: Int,
    startCharacter: Int,
    endLine: Int,
    endCharacter: Int
) extends SemanticdbMessage[Range] {
  @transient
  private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
  private[this] def __computeSerializedValue(): _root_.scala.Int = {
    var __size = 0

    {
      val __value = startLine
      if (__value != 0) {
        __size += SemanticdbOutputStream
          .computeInt32Size(1, __value)
      }
    };

    {
      val __value = startCharacter
      if (__value != 0) {
        __size += SemanticdbOutputStream
          .computeInt32Size(2, __value)
      }
    };

    {
      val __value = endLine
      if (__value != 0) {
        __size += SemanticdbOutputStream
          .computeInt32Size(3, __value)
      }
    };

    {
      val __value = endCharacter
      if (__value != 0) {
        __size += SemanticdbOutputStream
          .computeInt32Size(4, __value)
      }
    };
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
  def writeTo(
      `_output__`: SemanticdbOutputStream
  ): _root_.scala.Unit = {
    {
      val __v = startLine
      if (__v != 0) {
        _output__.writeInt32(1, __v)
      }
    };
    {
      val __v = startCharacter
      if (__v != 0) {
        _output__.writeInt32(2, __v)
      }
    };
    {
      val __v = endLine
      if (__v != 0) {
        _output__.writeInt32(3, __v)
      }
    };
    {
      val __v = endCharacter
      if (__v != 0) {
        _output__.writeInt32(4, __v)
      }
    };
  }
  def mergeFrom(`_input__`: SemanticdbInputStream): Range = {
    var __startLine = this.startLine
    var __startCharacter = this.startCharacter
    var __endLine = this.endLine
    var __endCharacter = this.endCharacter
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 8 =>
          __startLine = _input__.readInt32()
        case 16 =>
          __startCharacter = _input__.readInt32()
        case 24 =>
          __endLine = _input__.readInt32()
        case 32 =>
          __endCharacter = _input__.readInt32()
        case tag => _input__.skipField(tag)
      }
    }
    Range(
      startLine = __startLine,
      startCharacter = __startCharacter,
      endLine = __endLine,
      endCharacter = __endCharacter
    )
  }
}
