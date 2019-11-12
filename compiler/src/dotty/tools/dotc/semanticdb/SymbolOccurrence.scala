package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

object SymbolOccurrence {
  sealed abstract class Role(val value: Int) extends SemanticdbEnum {
    def isDefinition: Boolean = this == Role.DEFINITION
    def isReference: Boolean = this == Role.REFERENCE
  }

  object Role {
    case object UNKNOWN_ROLE extends Role(0)
    case object REFERENCE extends Role(1)
    case object DEFINITION extends Role(2)
    case class Unrecognized(id: Int) extends Role(id)

    def fromValue(value: Int): Role = value match {
      case 0 => UNKNOWN_ROLE
      case 1 => REFERENCE
      case 2 => DEFINITION
      case __other => Unrecognized(__other)
    }
  }

  def defaultInstance: SymbolOccurrence = SymbolOccurrence("", None, Role.UNKNOWN_ROLE)
}

case class SymbolOccurrence(
    symbol: String,
    range: Option[Range],
    role: SymbolOccurrence.Role
) extends SemanticdbMessage[SymbolOccurrence] {
  private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
  private[this] def __computeSerializedValue(): _root_.scala.Int = {
    var __size = 0
    if (range.isDefined) {
      val __value = range.get
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
        __value.serializedSize
      ) + __value.serializedSize
    };

    {
      val __value = symbol
      if (__value != "") {
        __size += SemanticdbOutputStream
          .computeStringSize(2, __value)
      }
    };

    {
      val __value = role
      if (__value != SymbolOccurrence.Role.UNKNOWN_ROLE) {
        __size += SemanticdbOutputStream
          .computeEnumSize(3, __value.value)
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
    range.foreach { __v =>
      val __m = __v
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
    {
      val __v = symbol
      if (__v != "") {
        _output__.writeString(2, __v)
      }
    };
    {
      val __v = role
      if (__v != SymbolOccurrence.Role.UNKNOWN_ROLE) {
        _output__.writeEnum(3, __v.value)
      }
    };
  }
  def mergeFrom(`_input__`: SemanticdbInputStream): SymbolOccurrence = {
    var __range = this.range
    var __symbol = this.symbol
    var __role = this.role
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 10 =>
          __range = Option(LiteParser.readMessage(_input__, __range.getOrElse(Range.defaultInstance)))
        case 18 =>
          __symbol = _input__.readString()
        case 24 =>
          __role = SymbolOccurrence.Role.fromValue(_input__.readEnum())
        case tag => _input__.skipField(tag)
      }
    }
    SymbolOccurrence(
      range = __range,
      symbol = __symbol,
      role = __role
    )
  }
}
