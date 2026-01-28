def tp[T](): T = ???

abstract class C:
  type T
  val x: T

def test: Unit =
  val x: Int = ???
  val z: Int = ???
  val c1: C = ???
  val c2: C = ???

  summon[{v: Int with v == x} <:< {v: Int with v == z}] // error
  summon[{v: C with v == c1} <:< {v: C with v == c2}] // error

  // summon[{v: Int with v == (??? : Int)} <:< {v: Int with v == (??? : Int)}] // TODO(mbovel): should not compare some impure applications?

  summon[{v: Int with v == tp[c1.T]()} <:< {v: Int with v == tp[c2.T]()}] // error
