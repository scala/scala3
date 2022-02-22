package dotty.tools
package dotc
package core

import Types._, Symbols._, Contexts._
import printing.Printer
import printing.Texts.Text

object Constants {

  inline val NoTag      = 0
  inline val UnitTag    = 1
  inline val BooleanTag = 2
  inline val ByteTag    = 3
  inline val ShortTag   = 4
  inline val CharTag    = 5
  inline val IntTag     = 6
  inline val LongTag    = 7
  inline val FloatTag   = 8
  inline val DoubleTag  = 9
  inline val StringTag  = 10
  inline val NullTag    = 11
  inline val ClazzTag   = 12

  class Constant(val value: Any, val tag: Int) extends printing.Showable with Product1[Any] {
    import java.lang.Double.doubleToRawLongBits
    import java.lang.Float.floatToRawIntBits

    def isByteRange: Boolean     = isIntRange && Byte.MinValue <= intValue && intValue <= Byte.MaxValue
    def isShortRange: Boolean    = isIntRange && Short.MinValue <= intValue && intValue <= Short.MaxValue
    def isCharRange: Boolean     = isIntRange && Char.MinValue <= intValue && intValue <= Char.MaxValue
    def isIntRange: Boolean      = ByteTag <= tag && tag <= IntTag
    def isLongRange: Boolean     = ByteTag <= tag && tag <= LongTag
    def isFloatRange: Boolean    = ByteTag <= tag && tag <= FloatTag
    def isNumeric: Boolean       = ByteTag <= tag && tag <= DoubleTag
    def isNonUnitAnyVal: Boolean = BooleanTag <= tag && tag <= DoubleTag
    def isAnyVal: Boolean        = UnitTag <= tag && tag <= DoubleTag

    def tpe(using Context): Type = tag match {
      case UnitTag        => defn.UnitType
      case BooleanTag     => defn.BooleanType
      case ByteTag        => defn.ByteType
      case ShortTag       => defn.ShortType
      case CharTag        => defn.CharType
      case IntTag         => defn.IntType
      case LongTag        => defn.LongType
      case FloatTag       => defn.FloatType
      case DoubleTag      => defn.DoubleType
      case StringTag      => defn.StringType
      case NullTag        => defn.NullType
      case ClazzTag       => defn.ClassType(typeValue)
    }

    /** We need the equals method to take account of tags as well as values.
     */
    override def equals(other: Any): Boolean = other match {
      case that: Constant =>
        this.tag == that.tag && equalHashValue == that.equalHashValue
      case _ => false
    }

    def isNaN: Boolean = value match {
      case f: Float  => f.isNaN
      case d: Double => d.isNaN
      case _ => false
    }

    def booleanValue: Boolean =
      if (tag == BooleanTag) value.asInstanceOf[Boolean]
      else throw new Error("value " + value + " is not a boolean")

    def byteValue: Byte = tag match {
      case ByteTag   => value.asInstanceOf[Byte]
      case ShortTag  => value.asInstanceOf[Short].toByte
      case CharTag   => value.asInstanceOf[Char].toByte
      case IntTag    => value.asInstanceOf[Int].toByte
      case LongTag   => value.asInstanceOf[Long].toByte
      case FloatTag  => value.asInstanceOf[Float].toByte
      case DoubleTag => value.asInstanceOf[Double].toByte
      case _         => throw new Error("value " + value + " is not a Byte")
    }

    def shortValue: Short = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toShort
      case ShortTag  => value.asInstanceOf[Short]
      case CharTag   => value.asInstanceOf[Char].toShort
      case IntTag    => value.asInstanceOf[Int].toShort
      case LongTag   => value.asInstanceOf[Long].toShort
      case FloatTag  => value.asInstanceOf[Float].toShort
      case DoubleTag => value.asInstanceOf[Double].toShort
      case _         => throw new Error("value " + value + " is not a Short")
    }

    def charValue: Char = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toChar
      case ShortTag  => value.asInstanceOf[Short].toChar
      case CharTag   => value.asInstanceOf[Char]
      case IntTag    => value.asInstanceOf[Int].toChar
      case LongTag   => value.asInstanceOf[Long].toChar
      case FloatTag  => value.asInstanceOf[Float].toChar
      case DoubleTag => value.asInstanceOf[Double].toChar
      case _         => throw new Error("value " + value + " is not a Char")
    }

    def intValue: Int = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toInt
      case ShortTag  => value.asInstanceOf[Short].toInt
      case CharTag   => value.asInstanceOf[Char].toInt
      case IntTag    => value.asInstanceOf[Int]
      case LongTag   => value.asInstanceOf[Long].toInt
      case FloatTag  => value.asInstanceOf[Float].toInt
      case DoubleTag => value.asInstanceOf[Double].toInt
      case _         => throw new Error("value " + value + " is not an Int")
    }

    def longValue: Long = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toLong
      case ShortTag  => value.asInstanceOf[Short].toLong
      case CharTag   => value.asInstanceOf[Char].toLong
      case IntTag    => value.asInstanceOf[Int].toLong
      case LongTag   => value.asInstanceOf[Long]
      case FloatTag  => value.asInstanceOf[Float].toLong
      case DoubleTag => value.asInstanceOf[Double].toLong
      case _         => throw new Error("value " + value + " is not a Long")
    }

    def floatValue: Float = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toFloat
      case ShortTag  => value.asInstanceOf[Short].toFloat
      case CharTag   => value.asInstanceOf[Char].toFloat
      case IntTag    => value.asInstanceOf[Int].toFloat
      case LongTag   => value.asInstanceOf[Long].toFloat
      case FloatTag  => value.asInstanceOf[Float]
      case DoubleTag => value.asInstanceOf[Double].toFloat
      case _         => throw new Error("value " + value + " is not a Float")
    }

    def doubleValue: Double = tag match {
      case ByteTag   => value.asInstanceOf[Byte].toDouble
      case ShortTag  => value.asInstanceOf[Short].toDouble
      case CharTag   => value.asInstanceOf[Char].toDouble
      case IntTag    => value.asInstanceOf[Int].toDouble
      case LongTag   => value.asInstanceOf[Long].toDouble
      case FloatTag  => value.asInstanceOf[Float].toDouble
      case DoubleTag => value.asInstanceOf[Double]
      case _         => throw new Error("value " + value + " is not a Double")
    }

    /** Convert constant value to conform to given type.
     */
    def convertTo(pt: Type)(using Context): Constant = {
      def classBound(pt: Type): Type = pt.dealias.stripTypeVar match {
        case tref: TypeRef if !tref.symbol.isClass && tref.info.exists =>
          classBound(tref.info.bounds.lo)
        case param: TypeParamRef =>
          ctx.typerState.constraint.entry(param) match {
            case TypeBounds(lo, hi) =>
              if (hi.classSymbol.isPrimitiveValueClass) hi //constrain further with high bound
              else classBound(lo)
            case NoType => classBound(param.binder.paramInfos(param.paramNum).lo)
            case inst => classBound(inst)
          }
        case pt => pt
      }
      pt match
        case ConstantType(value) if value == this => this
        case _: SingletonType => null
        case _ =>
          val target = classBound(pt).typeSymbol
          if (target == tpe.typeSymbol)
            this
          else if ((target == defn.ByteClass) && isByteRange)
            Constant(byteValue)
          else if (target == defn.ShortClass && isShortRange)
            Constant(shortValue)
          else if (target == defn.CharClass && isCharRange)
            Constant(charValue)
          else if (target == defn.IntClass && isIntRange)
            Constant(intValue)
          else if (target == defn.LongClass && isLongRange)
            Constant(longValue)
          else if (target == defn.FloatClass && isFloatRange)
            Constant(floatValue)
          else if (target == defn.DoubleClass && isNumeric)
            Constant(doubleValue)
          else
            null
    }

    def stringValue: String = value.toString

    def toText(printer: Printer): Text = printer.toText(this)

    def typeValue: Type     = value.asInstanceOf[Type]

    /**
     * Consider two `NaN`s to be identical, despite non-equality
     * Consider -0d to be distinct from 0d, despite equality
     *
     * We use the raw versions (i.e. `floatToRawIntBits` rather than `floatToIntBits`)
     * to avoid treating different encodings of `NaN` as the same constant.
     * You probably can't express different `NaN` varieties as compile time
     * constants in regular Scala code, but it is conceivable that you could
     * conjure them with a macro.
     */
    private def equalHashValue: Any = value match {
      case f: Float  => floatToRawIntBits(f)
      case d: Double => doubleToRawLongBits(d)
      case v         => v
    }

    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      val seed = 17
      var h = seed
      h = mix(h, tag.##) // include tag in the hash, otherwise 0, 0d, 0L, 0f collide.
      h = mix(h, equalHashValue.##)
      finalizeHash(h, length = 2)
    }

    override def toString: String = s"Constant($value)"
    def canEqual(x: Any): Boolean = true
    def get: Any                  = value
    def isEmpty: Boolean          = false
    def _1: Any                   = value
  }

  object Constant {
    def apply(x: Null): Constant         = new Constant(x, NullTag)
    def apply(x: Unit): Constant         = new Constant(x, UnitTag)
    def apply(x: Boolean): Constant      = new Constant(x, BooleanTag)
    def apply(x: Byte): Constant         = new Constant(x, ByteTag)
    def apply(x: Short): Constant        = new Constant(x, ShortTag)
    def apply(x: Int): Constant          = new Constant(x, IntTag)
    def apply(x: Long): Constant         = new Constant(x, LongTag)
    def apply(x: Float): Constant        = new Constant(x, FloatTag)
    def apply(x: Double): Constant       = new Constant(x, DoubleTag)
    def apply(x: String): Constant       = new Constant(x, StringTag)
    def apply(x: Char): Constant         = new Constant(x, CharTag)
    def apply(x: Type): Constant         = new Constant(x, ClazzTag)
    def apply(value: Any): Constant      =
      new Constant(value,
        value match {
          case null            => NullTag
          case x: Unit         => UnitTag
          case x: Boolean      => BooleanTag
          case x: Byte         => ByteTag
          case x: Short        => ShortTag
          case x: Int          => IntTag
          case x: Long         => LongTag
          case x: Float        => FloatTag
          case x: Double       => DoubleTag
          case x: String       => StringTag
          case x: Char         => CharTag
          case x: Type         => ClazzTag
        }
      )

    def unapply(c: Constant): Constant = c
  }
}
