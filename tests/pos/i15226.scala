// scalac: -Werror
class Proj { type State = String }

sealed trait ProjState:
  val a: Proj
  val b: a.State

object ProjState:
  def unapply(pj: ProjState): Some[(pj.a.type, pj.b.type)] = Some((pj.a, pj.b))

def test(pj: ProjState) = pj match
  case ProjState(p, s) =>
