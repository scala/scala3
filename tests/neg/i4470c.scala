object DuplicatedEnum {

  enum Maybe[+T] { // error
    case Some[T](x: T) extends Maybe[T]
  }

  enum Maybe[+T] { // error
    case Some[T](x: T) extends Maybe[T]
  }

  enum Color { // error
    case Red, Green, Blue
  }

  enum Color { // error
    case Red, Green, Blue
  }

  enum Tag[T] { // error
    case Int extends Tag[Int]
    case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T] // mix order of class and value
    case String extends Tag[String]
  }

  enum Tag[T] { // error
    case Int extends Tag[Int]
    case OfClass[T]()(using val tag: reflect.ClassTag[T]) extends Tag[T] // mix order of class and value
    case String extends Tag[String]
  }

}
