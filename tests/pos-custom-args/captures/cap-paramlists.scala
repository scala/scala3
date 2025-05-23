import language.experimental.captureChecking

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  def foo[A^ >: {y} <: {x},
          B^,
          C^ <: {x},
          D^,
          E^ <: {C},
          F^ <: {C},
          G^ <: {x, y},
          H^ >: {x} <: {x,y}, T, U >: {x}]()[I^ <: {y, G, H},
                                            J^ <: {O.z},
                                            K^ <: {x, O.z},
                                            L^ <: {x, y, O.z},
                                            M^ >: {x, y, O.z} <: {C},
                                            N^ >: {x} <: {x},
                                            O^ >: {O.z} <: {O.z}] = ???