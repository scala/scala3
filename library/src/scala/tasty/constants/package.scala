package scala.tasty

package object constants {

  object Unit {
    def unapply(arg: Constant)(implicit ctx: Context): Boolean = ctx.toolbox.unapplyUnit(arg)
  }

  object Null {
    def unapply(arg: Constant)(implicit ctx: Context): Boolean = ctx.toolbox.unapplyNull(arg)
  }

  object Boolean {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Boolean] = ctx.toolbox.unapplyBoolean(arg)
  }

  object Byte {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Byte] = ctx.toolbox.unapplyByte(arg)
  }

  object Char {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Char] = ctx.toolbox.unapplyChar(arg)
  }

  object Short {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Short] = ctx.toolbox.unapplyShort(arg)
  }

  object Int {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Int] = ctx.toolbox.unapplyInt(arg)
  }

  object Long {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Long] = ctx.toolbox.unapplyLong(arg)
  }

  object Float {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Float] = ctx.toolbox.unapplyFloat(arg)
  }

  object Double {
    def unapply(arg: Constant)(implicit ctx: Context): Option[Double] = ctx.toolbox.unapplyDouble(arg)
  }

  object String {
    def unapply(arg: Constant)(implicit ctx: Context): Option[String] = ctx.toolbox.unapplyString(arg)
  }
}
