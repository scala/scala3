object Test {

  inline def bar() =
    delegate match { // error
      case _: Int =>
    }

  {
    delegate for Int = 9
    bar()
  }

  bar()
}