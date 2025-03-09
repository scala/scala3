import language.experimental.captureChecking

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  trait CaptureSet:
    cap A >: y <: x
    cap B = x
    cap C <: {x}
    cap D
    cap E <: C
    cap F <: {C}
    cap G <: {x, y}
    cap H >: {x} <: {x,y}
    cap I = {y, G, H}
    cap J = {O.z}
    cap K = {x, O.z}
    cap L <: {x, y, O.z}
    cap M >: {x, y, O.z} <: C
    cap N >: x <: x
    cap O >: O.z <: O.z