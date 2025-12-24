package scala.quoted.compiletime.internal

import dotty.tools.dotc
import scala.quoted.compiletime as pub

/////// Constant ///////////////////////////////////////////////////////////////

type Constant = pub.Constant & ConstantImpl
sealed trait ConstantImpl { _self: pub.Constant => }
object ConstantImpl {
  def apply(c: dotc.core.Constants.Constant): ConstantImpl = c.tag match {
    case dotc.core.Constants.BooleanTag => BooleanConstantImpl(c.booleanValue)
    case dotc.core.Constants.ByteTag => ByteConstantImpl(c.byteValue)
    case dotc.core.Constants.ShortTag => ShortConstantImpl(c.shortValue)
    case dotc.core.Constants.IntTag => IntConstantImpl(c.intValue)
    case dotc.core.Constants.LongTag => LongConstantImpl(c.longValue)
    case dotc.core.Constants.FloatTag => FloatConstantImpl(c.floatValue)
    case dotc.core.Constants.DoubleTag => DoubleConstantImpl(c.doubleValue)
    case dotc.core.Constants.CharTag => CharConstantImpl(c.charValue)
    case dotc.core.Constants.StringTag => StringConstantImpl(c.stringValue)
    case dotc.core.Constants.UnitTag => UnitConstantImpl()
    case dotc.core.Constants.NullTag => NullConstantImpl()
    case dotc.core.Constants.ClazzTag => ClassOfConstantImpl(TypeReprImpl(c.typeValue))
  }

  object Module extends pub.Constant.Module {}

}

/////// BooleanConstant ///////////////////////////////////////////////////////////////

type BooleanConstant = BooleanConstantImpl
final case class BooleanConstantImpl(value: Boolean) extends ConstantImpl, pub.BooleanConstant
object BooleanConstantImpl {

  object Module extends pub.BooleanConstant.Module {
    override def apply(x: Boolean): BooleanConstant = BooleanConstantImpl(x)
    override def make(x: Boolean): BooleanConstant = BooleanConstantImpl(x)
  }

}

/////// ByteConstant ///////////////////////////////////////////////////////////////

type ByteConstant = ByteConstantImpl
final case class ByteConstantImpl(value: Byte) extends ConstantImpl, pub.ByteConstant
object ByteConstantImpl {

  object Module extends pub.ByteConstant.Module {
    override def apply(x: Byte): ByteConstant = ByteConstantImpl(x)
    override def make(x: Byte): ByteConstant = ByteConstantImpl(x)
  }

}

/////// ShortConstant ///////////////////////////////////////////////////////////////

type ShortConstant = ShortConstantImpl
final case class ShortConstantImpl(value: Short) extends ConstantImpl, pub.ShortConstant
object ShortConstantImpl {

  object Module extends pub.ShortConstant.Module {
    override def apply(x: Short): ShortConstant = ShortConstantImpl(x)
    override def make(x: Short): ShortConstant = ShortConstantImpl(x)
  }

}

/////// IntConstant ///////////////////////////////////////////////////////////////

type IntConstant = IntConstantImpl
final case class IntConstantImpl(value: Int) extends ConstantImpl, pub.IntConstant
object IntConstantImpl {

  object Module extends pub.IntConstant.Module {
    override def apply(x: Int): IntConstant = IntConstantImpl(x)
    override def make(x: Int): IntConstant = IntConstantImpl(x)
  }

}

/////// LongConstant ///////////////////////////////////////////////////////////////

type LongConstant = LongConstantImpl
final case class LongConstantImpl(value: Long) extends ConstantImpl, pub.LongConstant
object LongConstantImpl {

  object Module extends pub.LongConstant.Module {
    override def apply(x: Long): LongConstant = LongConstantImpl(x)
    override def make(x: Long): LongConstant = LongConstantImpl(x)
  }

}

/////// FloatConstant ///////////////////////////////////////////////////////////////

type FloatConstant = FloatConstantImpl
final case class FloatConstantImpl(value: Float) extends ConstantImpl, pub.FloatConstant
object FloatConstantImpl {

  object Module extends pub.FloatConstant.Module {
    override def apply(x: Float): FloatConstant = FloatConstantImpl(x)
    override def make(x: Float): FloatConstant = FloatConstantImpl(x)
  }

}

/////// DoubleConstant ///////////////////////////////////////////////////////////////

type DoubleConstant = DoubleConstantImpl
final case class DoubleConstantImpl(value: Double) extends ConstantImpl, pub.DoubleConstant
object DoubleConstantImpl {

  object Module extends pub.DoubleConstant.Module {
    override def apply(x: Double): DoubleConstant = DoubleConstantImpl(x)
    override def make(x: Double): DoubleConstant = DoubleConstantImpl(x)
  }

}

/////// CharConstant ///////////////////////////////////////////////////////////////

type CharConstant = CharConstantImpl
final case class CharConstantImpl(value: Char) extends ConstantImpl, pub.CharConstant
object CharConstantImpl {

  object Module extends pub.CharConstant.Module {
    override def apply(x: Char): CharConstant = CharConstantImpl(x)
    override def make(x: Char): CharConstant = CharConstantImpl(x)
  }

}

/////// StringConstant ///////////////////////////////////////////////////////////////

type StringConstant = StringConstantImpl
final case class StringConstantImpl(value: String) extends ConstantImpl, pub.StringConstant
object StringConstantImpl {

  object Module extends pub.StringConstant.Module {
    override def apply(x: String): StringConstant = StringConstantImpl(x)
    override def make(x: String): StringConstant = StringConstantImpl(x)
  }

}

/////// UnitConstant ///////////////////////////////////////////////////////////////

type UnitConstant = UnitConstantImpl
final case class UnitConstantImpl() extends ConstantImpl, pub.UnitConstant { override def value: Unit = () }
object UnitConstantImpl {

  object Module extends pub.UnitConstant.Module {
    override def apply(): UnitConstant = UnitConstantImpl()
    override def make(): UnitConstant = UnitConstantImpl()
  }

}

/////// NullConstant ///////////////////////////////////////////////////////////////

type NullConstant = NullConstantImpl
final case class NullConstantImpl() extends ConstantImpl, pub.NullConstant { override def value: Null = null }
object NullConstantImpl {

  object Module extends pub.NullConstant.Module {
    override def apply(): NullConstant = NullConstantImpl()
    override def make(): NullConstant = NullConstantImpl()
  }

}

/////// ClassOfConstant ///////////////////////////////////////////////////////////////

type ClassOfConstant = ClassOfConstantImpl
final case class ClassOfConstantImpl(value: TypeReprImpl) extends ConstantImpl, pub.ClassOfConstant
object ClassOfConstantImpl {

  object Module extends pub.ClassOfConstant.Module {
    override def apply(x: pub.TypeRepr): ClassOfConstant = ClassOfConstantImpl(x.asInstanceOf[TypeReprImpl])
    override def make(x: pub.TypeRepr): ClassOfConstant = ClassOfConstantImpl(x.asInstanceOf[TypeReprImpl])
  }

}