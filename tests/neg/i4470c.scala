object DuplicatedEnum {
  enum Maybe[+T] { // error // error
    case Some(x: T)
  }

  enum Maybe[+T] { // error
    case Some(x: T)
  }
}
