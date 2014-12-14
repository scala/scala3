abstract class NeedsXEarly {
  val x: Int
}
class Foo extends NeedsXEarly {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
val x = 1
// END copied early initializers
}
