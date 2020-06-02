object DuplicatedEnum {
  enum Maybe[+T] { // error
    case Some(x: T)
  }

  enum Maybe[+T] { // error
    case Some(x: T)
  }
}
