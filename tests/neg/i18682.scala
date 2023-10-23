class Foo(x: Int)

val _ = Fop(1)  // error
val _ = new Fooo(2) // error
val hello = "hi"
val _ = hellx // error

object Bar:
  class Baz()
  object App
  def cool = 1
  def wool = 2
  def pool = 3

val bar = Bar
val _ = bar.Bap // error, App does not show as hint, too far away
val _ = bar.Bap() // error

val _ = error // error, java.lang.Error does not show as hint, since it is not a value

// #17067
val _ = "123".view.reverse.padTo(5, '0').iterator.reverse // error, no hint since `reversed` is not accessible

val cool = "cool"
val wool = "wool"

val _ = pool // error

val _ = bar.poodle // error

val _ = bar.ool // error
