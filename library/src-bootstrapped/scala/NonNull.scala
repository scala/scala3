package scala

object NonNull {
  implicit class NonNull[T](x: T|Null) {
    def nn: T = x.asInstanceOf[T]
  }
}
