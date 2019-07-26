object Test {

  inline def bar() =
    delegate match { // error
      case _: Int =>
    }

  {
    given as Int = 9
    bar()
  }

  bar()
}