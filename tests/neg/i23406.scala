inline def funny[T]: String =
  inline compiletime.erasedValue[T] match
    case x: String => x

@main def Test = funny[String]  // error
