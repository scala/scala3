package dotty.tools
package dotc
package semanticdb

import dotty.tools.dotc.{semanticdb => s}

import core.Contexts.Context
import core.Constants._

object ConstantOps:
  extension (const: Constant)
    def toSemanticConst(using Context): s.Constant = const.tag match {
      case UnitTag => s.UnitConstant()
      case BooleanTag => s.BooleanConstant(const.booleanValue)
      case ByteTag => s.ByteConstant(const.byteValue)
      case ShortTag => s.ShortConstant(const.shortValue)
      case CharTag => s.CharConstant(const.charValue)
      case IntTag => s.IntConstant(const.intValue)
      case LongTag => s.LongConstant(const.longValue)
      case FloatTag => s.FloatConstant(const.floatValue)
      case DoubleTag => s.DoubleConstant(const.doubleValue)
      case StringTag => s.StringConstant(const.stringValue)
      case NullTag => s.NullConstant()
      // ConstantType(_: Type, ClazzTag) should be converted as it's type
      // NoTag => it shouldn't happen
      case _ => throw new Error(s"Constant ${const} can't be converted to Semanticdb Constant.")
    }
