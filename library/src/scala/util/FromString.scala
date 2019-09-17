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

  given FromString[String] {
    def fromString(s: String) = s
  }

  given FromString[Boolean] {
    def fromString(s: String) = s.toBoolean
  }

  given FromString[Byte] {
    def fromString(s: String) = s.toByte
  }

  given FromString[Short] {
    def fromString(s: String) = s.toShort
  }

  given FromString[Int] {
    def fromString(s: String) = s.toInt
  }

  given FromString[Long] {
    def fromString(s: String) = s.toLong
  }

  given FromString[Float] {
    def fromString(s: String) = s.toFloat
  }

  given FromString[Double] {
    def fromString(s: String) = s.toDouble
  }
}
