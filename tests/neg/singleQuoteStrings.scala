import language.experimental.dedentedStringLiterals

@main def Test =

  val a = '' // error
    hello
    world! // error
    '' // error

  val b = s'' // error
    hello
    ''  // error

