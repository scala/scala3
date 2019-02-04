object RepeatedExtendEnum {

  enum Maybe[T] derives Eq { // error // error
    case Foo extends Maybe[Int]
  }

  enum Maybe[T] derives Eq { // error
    case Foo extends Maybe[Int]
  }
}
