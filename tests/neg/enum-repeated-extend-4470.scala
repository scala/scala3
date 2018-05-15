object RepeatedExtendEnum {

  enum Maybe[T] { // error // error
    case Foo extends Maybe[Int]
  }

  enum Maybe[T] { // error
    case Foo extends Maybe[Int]
  }
}
