import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  def foo[cap A >: {y} <: {x},
          cap B,
          cap C <: {x},
          cap D : Ctx,
          cap E <: {C},
          cap F <: {C},
          cap G <: {x, y},
          cap H >: {x} <: {x,y} : Ctx, T, U]()[cap I <: {y, G, H},
                                               cap J <: {O.z},
                                               cap K <: {x, O.z},
                                               cap L <: {x, y, O.z},
                                               cap M >: {x, y, O.z} <: {C} : Ctx,
                                               cap N >: {x} <: {x},
                                               cap O >: {O.z} <: {O.z}] = ???