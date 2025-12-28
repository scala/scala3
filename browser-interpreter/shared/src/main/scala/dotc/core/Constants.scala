package dotc.core

/**
 * Cross-platform constant representation for the browser compiler.
 *
 * Constants represent literal values in the AST.
 */
object Constants {

  // Constant tags
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

  /**
   * A compile-time constant value.
   */
  class Constant(val value: Any, val tag: Int) extends Product1[Any] {

    // Range checks
    def isByteRange: Boolean = isIntRange && Byte.MinValue <= intValue && intValue <= Byte.MaxValue
    def isShortRange: Boolean = isIntRange && Short.MinValue <= intValue && intValue <= Short.MaxValue
    def isCharRange: Boolean = isIntRange && Char.MinValue <= intValue && intValue <= Char.MaxValue
    def isIntRange: Boolean = ByteTag <= tag && tag <= IntTag
    def isLongRange: Boolean = ByteTag <= tag && tag <= LongTag
    def isFloatRange: Boolean = ByteTag <= tag && tag <= FloatTag
    def isNumeric: Boolean = ByteTag <= tag && tag <= DoubleTag
    def isNonUnitAnyVal: Boolean = BooleanTag <= tag && tag <= DoubleTag
    def isAnyVal: Boolean = UnitTag <= tag && tag <= DoubleTag

    def isNaN: Boolean = value match {
      case f: Float => f.isNaN
      case d: Double => d.isNaN
      case _ => false
    }

    // Value accessors
    def booleanValue: Boolean = tag match {
      case BooleanTag => value.asInstanceOf[Boolean]
      case _ => throw new Error(s"value $value is not a Boolean")
    }

    def byteValue: Byte = tag match {
      case ByteTag => value.asInstanceOf[Byte]
      case ShortTag => value.asInstanceOf[Short].toByte
      case CharTag => value.asInstanceOf[Char].toByte
      case IntTag => value.asInstanceOf[Int].toByte
      case LongTag => value.asInstanceOf[Long].toByte
      case FloatTag => value.asInstanceOf[Float].toByte
      case DoubleTag => value.asInstanceOf[Double].toByte
      case _ => throw new Error(s"value $value is not a Byte")
    }

    def shortValue: Short = tag match {
      case ByteTag => value.asInstanceOf[Byte].toShort
      case ShortTag => value.asInstanceOf[Short]
      case CharTag => value.asInstanceOf[Char].toShort
      case IntTag => value.asInstanceOf[Int].toShort
      case LongTag => value.asInstanceOf[Long].toShort
      case FloatTag => value.asInstanceOf[Float].toShort
      case DoubleTag => value.asInstanceOf[Double].toShort
      case _ => throw new Error(s"value $value is not a Short")
    }

    def charValue: Char = tag match {
      case ByteTag => value.asInstanceOf[Byte].toChar
      case ShortTag => value.asInstanceOf[Short].toChar
      case CharTag => value.asInstanceOf[Char]
      case IntTag => value.asInstanceOf[Int].toChar
      case LongTag => value.asInstanceOf[Long].toChar
      case FloatTag => value.asInstanceOf[Float].toChar
      case DoubleTag => value.asInstanceOf[Double].toChar
      case _ => throw new Error(s"value $value is not a Char")
    }

    def intValue: Int = tag match {
      case ByteTag => value.asInstanceOf[Byte].toInt
      case ShortTag => value.asInstanceOf[Short].toInt
      case CharTag => value.asInstanceOf[Char].toInt
      case IntTag => value.asInstanceOf[Int]
      case LongTag => value.asInstanceOf[Long].toInt
      case FloatTag => value.asInstanceOf[Float].toInt
      case DoubleTag => value.asInstanceOf[Double].toInt
      case _ => throw new Error(s"value $value is not an Int")
    }

    def longValue: Long = tag match {
      case ByteTag => value.asInstanceOf[Byte].toLong
      case ShortTag => value.asInstanceOf[Short].toLong
      case CharTag => value.asInstanceOf[Char].toLong
      case IntTag => value.asInstanceOf[Int].toLong
      case LongTag => value.asInstanceOf[Long]
      case FloatTag => value.asInstanceOf[Float].toLong
      case DoubleTag => value.asInstanceOf[Double].toLong
      case _ => throw new Error(s"value $value is not a Long")
    }

    def floatValue: Float = tag match {
      case ByteTag => value.asInstanceOf[Byte].toFloat
      case ShortTag => value.asInstanceOf[Short].toFloat
      case CharTag => value.asInstanceOf[Char].toFloat
      case IntTag => value.asInstanceOf[Int].toFloat
      case LongTag => value.asInstanceOf[Long].toFloat
      case FloatTag => value.asInstanceOf[Float]
      case DoubleTag => value.asInstanceOf[Double].toFloat
      case _ => throw new Error(s"value $value is not a Float")
    }

    def doubleValue: Double = tag match {
      case ByteTag => value.asInstanceOf[Byte].toDouble
      case ShortTag => value.asInstanceOf[Short].toDouble
      case CharTag => value.asInstanceOf[Char].toDouble
      case IntTag => value.asInstanceOf[Int].toDouble
      case LongTag => value.asInstanceOf[Long].toDouble
      case FloatTag => value.asInstanceOf[Float].toDouble
      case DoubleTag => value.asInstanceOf[Double]
      case _ => throw new Error(s"value $value is not a Double")
    }

    def stringValue: String = value.toString

    // Equality - use intBits/longBits which are available in Scala.js
    private def equalHashValue: Any = value match {
      case f: Float => java.lang.Float.floatToIntBits(f)
      case d: Double => java.lang.Double.doubleToLongBits(d)
      case v => v
    }

    override def equals(other: Any): Boolean = other match {
      case that: Constant => this.tag == that.tag && equalHashValue == that.equalHashValue
      case _ => false
    }

    override def hashCode: Int = {
      var h = 17
      h = 31 * h + tag.hashCode
      h = 31 * h + equalHashValue.hashCode
      h
    }

    override def toString: String = s"Constant($value)"

    // Product1 implementation
    def canEqual(x: Any): Boolean = true
    def _1: Any = value
  }

  object Constant {
    def apply(x: Null): Constant = new Constant(x, NullTag)
    def apply(x: Unit): Constant = new Constant(x, UnitTag)
    def apply(x: Boolean): Constant = new Constant(x, BooleanTag)
    def apply(x: Byte): Constant = new Constant(x, ByteTag)
    def apply(x: Short): Constant = new Constant(x, ShortTag)
    def apply(x: Int): Constant = new Constant(x, IntTag)
    def apply(x: Long): Constant = new Constant(x, LongTag)
    def apply(x: Float): Constant = new Constant(x, FloatTag)
    def apply(x: Double): Constant = new Constant(x, DoubleTag)
    def apply(x: String): Constant = new Constant(x, StringTag)
    def apply(x: Char): Constant = new Constant(x, CharTag)

    def apply(value: Any): Constant = new Constant(value, value match {
      case null => NullTag
      case _: Unit => UnitTag
      case _: Boolean => BooleanTag
      case _: Byte => ByteTag
      case _: Short => ShortTag
      case _: Int => IntTag
      case _: Long => LongTag
      case _: Float => FloatTag
      case _: Double => DoubleTag
      case _: String => StringTag
      case _: Char => CharTag
      case _ => NoTag
    })

    def unapply(c: Constant): Constant = c
  }
}

