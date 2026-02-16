import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???

  abstract class Foo[A^, T]:
    type C^ <: {A}
  abstract class Bar extends Foo[{x,y,O.z}, String]:
    override type C^ <: {x,y}
  class Baz extends Bar:
    final override type C = {y}