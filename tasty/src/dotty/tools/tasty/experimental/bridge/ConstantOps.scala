package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait ConstantOps extends Core with

  object Constants with
    final val NoTag      = internal.Constants_NoTag
    final val UnitTag    = internal.Constants_UnitTag
    final val BooleanTag = internal.Constants_BooleanTag
    final val ByteTag    = internal.Constants_ByteTag
    final val ShortTag   = internal.Constants_ShortTag
    final val CharTag    = internal.Constants_CharTag
    final val IntTag     = internal.Constants_IntTag
    final val LongTag    = internal.Constants_LongTag
    final val FloatTag   = internal.Constants_FloatTag
    final val DoubleTag  = internal.Constants_DoubleTag
    final val StringTag  = internal.Constants_StringTag
    final val NullTag    = internal.Constants_NullTag
    final val ClazzTag   = internal.Constants_ClazzTag
    final val EnumTag    = internal.Constants_EnumTag
  end Constants

  given ConstantOps: (c: Constant) with
    def tag: Int = internal.Constant_tag(c)
    def intValue: Int = internal.Constant_intValue(c)
    def booleanValue: Boolean = internal.Constant_booleanValue(c)
    def shortValue: Short = internal.Constant_shortValue(c)
    def charValue: Char = internal.Constant_charValue(c)
    def byteValue: Byte = internal.Constant_byteValue(c)
    def longValue: Long = internal.Constant_longValue(c)
    def floatValue: Float = internal.Constant_floatValue(c)
    def doubleValue: Double = internal.Constant_doubleValue(c)
    def stringValue: String = internal.Constant_stringValue(c)
    def typeValue: Type = internal.Constant_typeValue(c)
    def symbolValue: Symbol = internal.Constant_symbolValue(c)
  end ConstantOps
