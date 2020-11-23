object RepeatedExtendEnum {

  enum Maybe[T] derives CanEqual { // error
    case Foo extends Maybe[Int]
  }

  enum Maybe[T] derives CanEqual { // error
    case Foo extends Maybe[Int]
  }
}
