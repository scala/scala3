object Test {

  inline def bar() =
    compiletime.summonFrom {  // error
      case _: Int =>
    }

  {
    given as Int = 9
    bar()
  }

  bar()
}