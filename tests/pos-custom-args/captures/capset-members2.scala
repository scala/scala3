import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???

  abstract class Foo[cap A, T]:
    cap type C <: {A}
  abstract class Bar extends Foo[{x,y,O.z}, String]:
    override cap type C <: {x,y}
  class Baz extends Bar:
    final override cap type C = {y}