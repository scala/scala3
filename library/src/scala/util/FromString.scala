package scala.util

trait FromString[T] {
  /** Can throw java.lang.IllegalArgumentException */
  def fromString(s: String): T

  def fromStringOption(s: String): Option[T] =
    try Some(fromString(s))
    catch {
      case ex: IllegalArgumentException => None
    }
}

object FromString {

  delegate for FromString[String] {
    def fromString(s: String) = s
  }

  delegate for FromString[Boolean] {
    def fromString(s: String) = s.toBoolean
  }

  delegate for FromString[Byte] {
    def fromString(s: String) = s.toByte
  }

  delegate for FromString[Short] {
    def fromString(s: String) = s.toShort
  }

  delegate for FromString[Int] {
    def fromString(s: String) = s.toInt
  }

  delegate for FromString[Long] {
    def fromString(s: String) = s.toLong
  }

  delegate for FromString[Float] {
    def fromString(s: String) = s.toFloat
  }

  delegate for FromString[Double] {
    def fromString(s: String) = s.toDouble
  }
}
