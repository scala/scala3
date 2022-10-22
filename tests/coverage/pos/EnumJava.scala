package covtest

enum MyLogLevel extends java.lang.Enum[MyLogLevel]:
  case Warn  extends MyLogLevel
  case Error extends MyLogLevel
  case Fatal extends MyLogLevel
