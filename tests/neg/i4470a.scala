object RepeatedEnum {

  enum Maybe { // error
    case Foo   // error
  }

  enum Maybe { // error
    case Foo
  }
}
