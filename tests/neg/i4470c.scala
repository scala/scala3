object DuplicatedEnum {
  enum Maybe[+T] { // error
    case Some[T](x: T) extends Maybe[T]
  }

  enum Maybe[+T] { // error
    case Some[T](x: T) extends Maybe[T]
  }
}
