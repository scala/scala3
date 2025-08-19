import language.experimental.into

into class Test

into trait T

into object M   // error

object Test:
  into def foo = 22 // error
  into val x = 33   // error
  into type T = Int // error
  into opaque type U = Int // ok

