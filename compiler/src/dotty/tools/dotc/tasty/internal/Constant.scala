package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Constants

import scala.tasty.constants

object Constant {

  def apply(constant: Constants.Constant): constants.Constant = Impl(constant)

  def unapplyUnit(const: constants.Constant): Boolean = const match {
    case Impl(const) => const.tag == Constants.UnitTag
    case _ => false
  }

  def unapplyNull(const: constants.Constant): Boolean = const match {
    case Impl(const) => const.tag == Constants.NullTag
    case _ => false
  }

  def unapplyBoolean(const: constants.Constant): Option[Boolean] = const match {
    case Impl(const) if const.tag == Constants.BooleanTag => Some(const.booleanValue)
    case _ => None
  }

  def unapplyByte(const: constants.Constant): Option[Byte] = const match {
    case Impl(const) if const.tag == Constants.ByteTag => Some(const.byteValue)
    case _ => None
  }

  def unapplyChar(const: constants.Constant): Option[Char] = const match {
    case Impl(const) if const.tag == Constants.CharTag => Some(const.charValue)
    case _ => None
  }

  def unapplyShort(const: constants.Constant): Option[Short] = const match {
    case Impl(const) if const.tag == Constants.ShortTag => Some(const.shortValue)
    case _ => None
  }

  def unapplyInt(const: constants.Constant): Option[Int] = const match {
    case Impl(const) if const.tag == Constants.IntTag => Some(const.intValue)
    case _ => None
  }

  def unapplyLong(const: constants.Constant): Option[Long] = const match {
    case Impl(const) if const.tag == Constants.LongTag => Some(const.longValue)
    case _ => None
  }

  def unapplyFloat(const: constants.Constant): Option[Float] = const match {
    case Impl(const) if const.tag == Constants.FloatTag => Some(const.floatValue)
    case _ => None
  }

  def unapplyDouble(const: constants.Constant): Option[Double] = const match {
    case Impl(const) if const.tag == Constants.DoubleTag => Some(const.doubleValue)
    case _ => None
  }

  def unapplyString(const: constants.Constant): Option[String] = const match {
    case Impl(const) if const.tag == Constants.StringTag => Some(const.stringValue)
    case _ => None
  }

  private[tasty] case class Impl(const: Constants.Constant) extends constants.Constant {

    def value: Any = const.value

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case constants.Unit() => "Unit()"
        case constants.Null() => "Null()"
        case constants.Boolean(value) => s"Boolean($value)"
        case constants.Byte(value) => s"Byte($value)"
        case constants.Short(value) => s"Short($value)"
        case constants.Char(value) => s"Char('$value')"
        case constants.Int(value) => s"Int($value)"
        case constants.Long(value) => s"Long($value)"
        case constants.Float(value) => s"Float($value)"
        case constants.Double(value) => s"Double($value)"
        case constants.String(value) => s"""String("$value")"""
      }
    }
  }
}
