object DuplicatedEnum {
  enum Maybe[+T] { // error
    case Some(x: T) // error
  }

  enum Maybe[+T] { // error
    case Some(x: T)
  }
}
