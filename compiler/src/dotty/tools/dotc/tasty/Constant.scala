package dotty.tools.dotc.tasty

import dotty.tools.dotc.core.Constants

object Constant {

  def apply(constant: Constants.Constant): scala.tasty.Constant = Impl(constant)

  object Unit {
    def unapply(const: scala.tasty.Constant): Boolean = const match {
      case Impl(const) => const.tag == Constants.UnitTag
      case _ => false
    }
  }

  object Null {
    def unapply(const: scala.tasty.Constant): Boolean = const match {
      case Impl(const) => const.tag == Constants.NullTag
      case _ => false
    }
  }

  object Boolean {
    def unapply(const: scala.tasty.Constant): Option[Boolean] = const match {
      case Impl(const) if const.tag == Constants.BooleanTag => Some(const.booleanValue)
      case _ => None
    }
  }

  object Byte {
    def unapply(const: scala.tasty.Constant): Option[Byte] = const match {
      case Impl(const) if const.tag == Constants.ByteTag => Some(const.byteValue)
      case _ => None
    }
  }

  object Char {
    def unapply(const: scala.tasty.Constant): Option[Char] = const match {
      case Impl(const) if const.tag == Constants.CharTag => Some(const.charValue)
      case _ => None
    }
  }

  object Short {
    def unapply(const: scala.tasty.Constant): Option[Short] = const match {
      case Impl(const) if const.tag == Constants.ShortTag => Some(const.shortValue)
      case _ => None
    }
  }

  object Int {
    def unapply(const: scala.tasty.Constant): Option[Int] = const match {
      case Impl(const) if const.tag == Constants.IntTag => Some(const.intValue)
      case _ => None
    }
  }

  object Long {
    def unapply(const: scala.tasty.Constant): Option[Long] = const match {
      case Impl(const) if const.tag == Constants.LongTag => Some(const.longValue)
      case _ => None
    }
  }

  object Float {
    def unapply(const: scala.tasty.Constant): Option[Float] = const match {
      case Impl(const) if const.tag == Constants.FloatTag => Some(const.floatValue)
      case _ => None
    }
  }

  object Double {
    def unapply(const: scala.tasty.Constant): Option[Double] = const match {
      case Impl(const) if const.tag == Constants.DoubleTag => Some(const.doubleValue)
      case _ => None
    }
  }

  object String {
    def unapply(const: scala.tasty.Constant): Option[String] = const match {
      case Impl(const) if const.tag == Constants.StringTag => Some(const.stringValue)
      case _ => None
    }
  }

  private[tasty] case class Impl(const: Constants.Constant) extends scala.tasty.Constant {
    override def toString: String = this match {
      case Unit() => "Unit()"
      case Null() => "Null()"
      case Boolean(value) => s"Boolean($value)"
      case Byte(value) => s"Byte($value)"
      case Short(value) => s"Short($value)"
      case Char(value) => s"Char('$value')"
      case Int(value) => s"Int($value)"
      case Long(value) => s"Long($value)"
      case Float(value) => s"Float($value)"
      case Double(value) => s"Double($value)"
      case String(value) => s"""String("$value")"""
    }
  }
}
