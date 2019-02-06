object RepeatedExtendEnum {

  enum Maybe[T] derives Eql { // error // error
    case Foo extends Maybe[Int]
  }

  enum Maybe[T] derives Eql { // error
    case Foo extends Maybe[Int]
  }
}
