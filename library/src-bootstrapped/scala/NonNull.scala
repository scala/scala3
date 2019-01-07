package scala

object NonNull {
  implicit class NonNull[T](x: T|Null) {
    def nn: T = if (x == null) {
      throw new NullPointerException("tried cast away nullability, but value is null")
    } else {
      x.asInstanceOf[T]
    }
  }

  object ArrayConversions {
    // TODO(abeln) add additional versions of these a la FunctionN?
    implicit def toNullable1[T](a: Array[T]): Array[T|Null] = a.asInstanceOf[Array[T|Null]]
    implicit def toNullable2[T](a: Array[Array[T]]): Array[Array[T|Null]|Null] = a.asInstanceOf[Array[Array[T|Null]|Null]] 

    implicit def fromNullable1[T](a: Array[T|Null]): Array[T] = a.asInstanceOf[Array[T]]
    implicit def fromNullable2[T](a: Array[Array[T|Null]|Null]): Array[Array[T]] = a.asInstanceOf[Array[Array[T]]]
  }
}
