import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  trait CaptureSet:
    cap type A >: {y} <: {x}
    cap type B = {x}
    cap type C <: {x}
    cap type D : Ctx
    cap type E <: {C}
    cap type F <: {C}
    cap type G <: {x, y}
    cap type H >: {x} <: {x,y} : Ctx
    cap type I = {y, G, H}
    cap type J = {O.z}
    cap type K = {x, O.z}
    cap type L <: {x, y, O.z}
    cap type M >: {x, y, O.z} <: {C}
    cap type N >: {x} <: {x}
    cap type O >: {O.z} <: {O.z}