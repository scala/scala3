object Test {

  inline def bar() =
    compiletime.summonFrom {
      case _: Int =>
    }

  {
    given Int = 9
    bar()
  }

  bar() // error
}