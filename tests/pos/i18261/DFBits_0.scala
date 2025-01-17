trait DFBits[W <: Int]

trait Candidate[R]:
  type OutW <: Int
object Candidate:
  given [W <: Int, R <: Foo[DFBits[W]]] => Candidate[R]:
    type OutW = W
