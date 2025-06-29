import language.experimental.erasedDefinitions

def foo(erased x: String): String = ""

inline def funny[T]: String =
  inline compiletime.erasedValue[T] match
    case x: String => x

inline def problem[T]: String =
  inline compiletime.erasedValue[T] match
    case x: String => foo(x)

inline def ok[T]: String =
  inline compiletime.erasedValue[T] match
    case x: String => "hi"

def Test =
  funny[String]  // error
  problem[String] // error
  ok[String]
