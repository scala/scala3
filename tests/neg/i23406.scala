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
inline def alsoOk[T](erased x: T): String =
  inline x match
    case x: String => "hi again"

def Test =
  funny[String]  // error
  problem[String] // error
  ok[String]
  alsoOk[String](compiletime.erasedValue)
