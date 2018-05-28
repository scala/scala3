object RepeatedEnum {

  enum Maybe { // error // error
    case Foo
  }

  enum Maybe { // error
    case Foo
  }
}
