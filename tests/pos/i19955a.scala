
trait Summon[R, T <: R]:
  type Out
object Summon:
  given [R, T <: R]: Summon[R, T] with
    type Out = R

trait DFTypeAny
trait DFBits[W <: Int] extends DFTypeAny
class DFVal[+T <: DFTypeAny]
type DFValAny = DFVal[DFTypeAny]
type DFValOf[+T <: DFTypeAny] = DFVal[T]
trait Candidate[R]:
  type OutW <: Int
object Candidate:
  type Aux[R, O <: Int] = Candidate[R] { type OutW = O }
  given [W <: Int, R <: DFValOf[DFBits[W]]]: Candidate[R] with
    type OutW = W

extension [L](lhs: L) def foo(using es: Summon[L, lhs.type]): Unit = ???
extension [L <: DFValAny](lhs: L)(using icL: Candidate[L]) def baz: DFValOf[DFBits[icL.OutW]] = ???
extension [L <: DFValAny, W <: Int](lhs: L)(using icL: Candidate.Aux[L, W])
  def bazAux: DFValOf[DFBits[W]] = ???

val x = new DFVal[DFBits[4]]
val works = x.bazAux.foo
val fails = x.baz.foo