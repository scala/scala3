package scala

object NonNull {
  implicit class NonNull[T](x: T|Null) {
    def nn: T = if (x == null) {
      throw new NullPointerException("tried cast away nullability, but value is null")
    } else {
      x.asInstanceOf[T]
    }
  }
}
