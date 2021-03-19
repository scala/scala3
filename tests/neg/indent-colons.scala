import language.experimental.fewerBraces

val x = 1.+ :  // ok
  2

val y = 1 + : // error // error
  2  // error
