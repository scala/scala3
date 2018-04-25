package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Constants

import scala.tasty.constants

object Constant {

  def apply(constant: Constants.Constant): constants.Constant = new Impl(constant)

  def unapplyUnit(arg: Impl): Boolean = arg.const.tag == Constants.UnitTag

  def unapplyNull(arg: Impl): Boolean = arg.const.tag == Constants.NullTag

  def unapplyBoolean(arg: Impl): Option[Boolean] =
    if (arg.const.tag == Constants.BooleanTag) Some(arg.const.booleanValue)
    else None

  def unapplyByte(arg: Impl): Option[Byte] =
    if (arg.const.tag == Constants.ByteTag) Some(arg.const.byteValue)
    else None

  def unapplyChar(arg: Impl): Option[Char] =
    if (arg.const.tag == Constants.CharTag) Some(arg.const.charValue)
    else None

  def unapplyShort(arg: Impl): Option[Short] =
    if (arg.const.tag == Constants.ShortTag) Some(arg.const.shortValue)
    else None

  def unapplyInt(arg: Impl): Option[Int] =
    if (arg.const.tag == Constants.IntTag) Some(arg.const.intValue)
    else None

  def unapplyLong(arg: Impl): Option[Long] =
    if (arg.const.tag == Constants.LongTag) Some(arg.const.longValue)
    else None

  def unapplyFloat(arg: Impl): Option[Float] =
    if (arg.const.tag == Constants.FloatTag) Some(arg.const.floatValue)
    else None

  def unapplyDouble(arg: Impl): Option[Double] =
    if (arg.const.tag == Constants.DoubleTag) Some(arg.const.doubleValue)
    else None

  def unapplyString(arg: Impl): Option[String] =
    if (arg.const.tag == Constants.StringTag) Some(arg.const.stringValue)
    else None

  private[tasty] class Impl(val const: Constants.Constant) extends constants.Constant {

    def value: Any = const.value

    override def toString: String = "Constant"
  }
}
