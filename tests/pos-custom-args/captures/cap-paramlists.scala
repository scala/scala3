import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  def foo[cap A >: y <: x,
              B,
              C <: {x},
              D : Ctx,
              E <: C,
              F <: {C},
              G <: {x, y},
              H >: {x} <: {x,y} : Ctx]()[cap I <: {y, G, H},
                                             J <: O.z,
                                             K <: {x, O.z},
                                             L <: {x, y, O.z},
                                             M >: {x, y, O.z} <: C : Ctx,
                                             N >: x <: x,
                                             O >: O.z <: O.z] = ???