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

  given as FromString[String] {
    def fromString(s: String) = s
  }

  given as FromString[Boolean] {
    def fromString(s: String) = s.toBoolean
  }

  given as FromString[Byte] {
    def fromString(s: String) = s.toByte
  }

  given as FromString[Short] {
    def fromString(s: String) = s.toShort
  }

  given as FromString[Int] {
    def fromString(s: String) = s.toInt
  }

  given as FromString[Long] {
    def fromString(s: String) = s.toLong
  }

  given as FromString[Float] {
    def fromString(s: String) = s.toFloat
  }

  given as FromString[Double] {
    def fromString(s: String) = s.toDouble
  }
}
