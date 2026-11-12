//> using options -Werror -Wunreasonable-catch

// The fromOrdinal synthesized for an enum whose cases are all singletons
// mustn't trip -Wunreasonable-catch

enum Simple:
  case A, B

enum Java extends java.lang.Enum[Java]:
  case A, B

enum Mixed:
  case A
  case B(x: Int)
