class Foo(x: Int)

val _ = Fop(1)  // error
val _ = new Fooo(2) // error
val hello = "hi"
val _ = hellx // error

object Bar:
  class Baz()
  object App

val bar = Bar
val _ = bar.Bap // error, App does shown as hint, too far away
val _ = bar.Bap() // error

val _ = error // error, java.lang.Error does not show as hint, since it is not a value