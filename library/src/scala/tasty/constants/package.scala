package scala.tasty

import scala.runtime.tasty.Toolbox

package object constants {

  object Unit {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Boolean = toolbox.unapplyUnit(arg)
  }

  object Null {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Boolean = toolbox.unapplyNull(arg)
  }

  object Boolean {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Boolean] = toolbox.unapplyBoolean(arg)
  }

  object Byte {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Byte] = toolbox.unapplyByte(arg)
  }

  object Char {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Char] = toolbox.unapplyChar(arg)
  }

  object Short {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Short] = toolbox.unapplyShort(arg)
  }

  object Int {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Int] = toolbox.unapplyInt(arg)
  }

  object Long {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Long] = toolbox.unapplyLong(arg)
  }

  object Float {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Float] = toolbox.unapplyFloat(arg)
  }

  object Double {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[Double] = toolbox.unapplyDouble(arg)
  }

  object String {
    def unapply(arg: Constant)(implicit toolbox: Toolbox): Option[String] = toolbox.unapplyString(arg)
  }
}
