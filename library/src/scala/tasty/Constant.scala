package scala.tasty

trait Constant

object Unit {
  def unapply(arg: Constant)(implicit ext: Extractor): Boolean = ext.unapplyUnit(arg)
}

object Null {
  def unapply(arg: Constant)(implicit ext: Extractor): Boolean = ext.unapplyNull(arg)
}

object Boolean {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Boolean] = ext.unapplyBoolean(arg)
}

object Byte {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Byte] = ext.unapplyByte(arg)
}

object Char {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Char] = ext.unapplyChar(arg)
}

object Short {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Short] = ext.unapplyShort(arg)
}

object Int {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Int] = ext.unapplyInt(arg)
}

object Long {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Long] = ext.unapplyLong(arg)
}

object Float {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Float] = ext.unapplyFloat(arg)
}

object Double {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[Double] = ext.unapplyDouble(arg)
}

object String {
  def unapply(arg: Constant)(implicit ext: Extractor): Option[String] = ext.unapplyString(arg)
}
