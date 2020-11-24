package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._
import scala.annotation.internal.sharable

object SymbolInformation {

  val defaultInstance = SymbolInformation("", Language.UNKNOWN_LANGUAGE, SymbolInformation.Kind.UNKNOWN_KIND, 0, "")

  sealed trait Kind(val value: Int) extends SemanticdbEnum derives CanEqual {
    def isUnknownKind: Boolean = this == Kind.UNKNOWN_KIND
    def isLocal: Boolean = this == Kind.LOCAL
    def isField: Boolean = this == Kind.FIELD
    def isMethod: Boolean = this == Kind.METHOD
    def isConstructor: Boolean = this == Kind.CONSTRUCTOR
    def isMacro: Boolean = this == Kind.MACRO
    def isType: Boolean = this == Kind.TYPE
    def isParameter: Boolean = this == Kind.PARAMETER
    def isSelfParameter: Boolean = this == Kind.SELF_PARAMETER
    def isTypeParameter: Boolean = this == Kind.TYPE_PARAMETER
    def isObject: Boolean = this == Kind.OBJECT
    def isPackage: Boolean = this == Kind.PACKAGE
    def isPackageObject: Boolean = this == Kind.PACKAGE_OBJECT
    def isClass: Boolean = this == Kind.CLASS
    def isTrait: Boolean = this == Kind.TRAIT
    def isInterface: Boolean = this == Kind.INTERFACE
  }

  object Kind {

    case object UNKNOWN_KIND extends Kind(0)
    case object METHOD extends Kind(3)
    case object MACRO extends Kind(6)
    case object TYPE extends Kind(7)
    case object PARAMETER extends Kind(8)
    case object TYPE_PARAMETER extends Kind(9)
    case object OBJECT extends Kind(10)
    case object PACKAGE extends Kind(11)
    case object PACKAGE_OBJECT extends Kind(12)
    case object CLASS extends Kind(13)
    case object TRAIT extends Kind(14)
    case object SELF_PARAMETER extends Kind(17)
    case object INTERFACE extends Kind(18)
    case object LOCAL extends Kind(19)
    case object FIELD extends Kind(20)
    case object CONSTRUCTOR extends Kind(21)
    final case class Unrecognized(id: Int) extends Kind(id)

    def fromValue(value: Int): Kind = value match {
      case 0 => UNKNOWN_KIND
      case 3 => METHOD
      case 6 => MACRO
      case 7 => TYPE
      case 8 => PARAMETER
      case 9 => TYPE_PARAMETER
      case 10 => OBJECT
      case 11 => PACKAGE
      case 12 => PACKAGE_OBJECT
      case 13 => CLASS
      case 14 => TRAIT
      case 17 => SELF_PARAMETER
      case 18 => INTERFACE
      case 19 => LOCAL
      case 20 => FIELD
      case 21 => CONSTRUCTOR
      case id => Unrecognized(id)
    }
  }

  sealed trait Property(val value: Int) extends SemanticdbEnum derives CanEqual {
    def isUnknownProperty: Boolean = this == Property.UNKNOWN_PROPERTY
    def isAbstract: Boolean = this == Property.ABSTRACT
    def isFinal: Boolean = this == Property.FINAL
    def isSealed: Boolean = this == Property.SEALED
    def isImplicit: Boolean = this == Property.IMPLICIT
    def isLazy: Boolean = this == Property.LAZY
    def isCase: Boolean = this == Property.CASE
    def isCovariant: Boolean = this == Property.CONTRAVARIANT
    def isContravariant: Boolean = this == Property.CONTRAVARIANT
    def isVal: Boolean = this == Property.VAL
    def isVar: Boolean = this == Property.VAR
    def isStatic: Boolean = this == Property.STATIC
    def isPrimary: Boolean = this == Property.PRIMARY
    def isEnum: Boolean = this == Property.ENUM
    def isDefault: Boolean = this == Property.DEFAULT
  }

  object Property {

    case object UNKNOWN_PROPERTY extends Property(0)
    case object ABSTRACT extends Property(4)
    case object FINAL extends Property(8)
    case object SEALED extends Property(16)
    case object IMPLICIT extends Property(32)
    case object LAZY extends Property(64)
    case object CASE extends Property(128)
    case object COVARIANT extends Property(256)
    case object CONTRAVARIANT extends Property(512)
    case object VAL extends Property(1024)
    case object VAR extends Property(2048)
    case object STATIC extends Property(4096)
    case object PRIMARY extends Property(8192)
    case object ENUM extends Property(16384)
    case object DEFAULT extends Property(32768)
    final case class Unrecognized(id: Int) extends Property(id)

    def fromValue(value: Int): Property = value match {
      case 0x0 => UNKNOWN_PROPERTY
      case 0x4 => ABSTRACT
      case 0x8 => FINAL
      case 0x10 => SEALED
      case 0x20 => IMPLICIT
      case 0x40 => LAZY
      case 0x80 => CASE
      case 0x100 => COVARIANT
      case 0x200 => CONTRAVARIANT
      case 0x400 => VAL
      case 0x800 => VAR
      case 0x1000 => STATIC
      case 0x2000 => PRIMARY
      case 0x4000 => ENUM
      case 0x8000 => DEFAULT
      case id => Unrecognized(id)
    }
  }
}

final case class SymbolInformation(
  symbol: String,
  language: Language,
  kind: SymbolInformation.Kind,
  properties: Int,
  displayName: String
) extends SemanticdbMessage[SymbolInformation] derives CanEqual {
    @sharable
    private var __serializedSizeCachedValue: Int = 0
    private def __computeSerializedValue(): Int = {
      var __size = 0

      {
        val __value = symbol
        if (__value != "") {
          __size += SemanticdbOutputStream.computeStringSize(1, __value)
        }
      };

      {
        val __value = language
        if (__value != Language.UNKNOWN_LANGUAGE) {
          __size += SemanticdbOutputStream.computeEnumSize(16, __value.value)
        }
      };

      {
        val __value = kind
        if (__value != SymbolInformation.Kind.UNKNOWN_KIND) {
          __size += SemanticdbOutputStream.computeEnumSize(3, __value.value)
        }
      };

      {
        val __value = properties
        if (__value != 0) {
          __size += SemanticdbOutputStream.computeInt32Size(4, __value)
        }
      };

      {
        val __value = displayName
        if (__value != "") {
          __size += SemanticdbOutputStream.computeStringSize(5, __value)
        }
      };
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
        val __v = symbol
        if (__v != "") {
          _output__.writeString(1, __v)
        }
      };
      {
        val __v = kind
        if (__v != SymbolInformation.Kind.UNKNOWN_KIND) {
          _output__.writeEnum(3, __v.value)
        }
      };
      {
        val __v = properties
        if (__v != 0) {
          _output__.writeInt32(4, __v)
        }
      };
      {
        val __v = displayName
        if (__v != "") {
          _output__.writeString(5, __v)
        }
      };
      {
        val __v = language
        if (__v != Language.UNKNOWN_LANGUAGE) {
          _output__.writeEnum(16, __v.value)
        }
      };
    }
    def mergeFrom(`_input__`: SemanticdbInputStream): SymbolInformation = {
      var __symbol = this.symbol
      var __language = this.language
      var __kind = this.kind
      var __properties = this.properties
      var __displayName = this.displayName
      var _done__ = false
      while (!_done__) {
        val _tag__ = _input__.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __symbol = _input__.readString()
          case 24 =>
            __kind = SymbolInformation.Kind.fromValue(_input__.readEnum())
          case 32 =>
            __properties = _input__.readInt32()
          case 42 =>
            __displayName = _input__.readString()
          case tag => _input__.skipField(tag)
        }
      }
      SymbolInformation(
        symbol = __symbol,
        language = __language,
        kind = __kind,
        properties = __properties,
        displayName = __displayName
      )
    }
}
