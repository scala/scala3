
trait Summon[R, T <: R]:
  type Out
object Summon:
  given [R, T <: R]: Summon[R, T] with
    type Out = R

sealed class Modifier[+A, +P]
type ModifierAny = Modifier[Any, Any]
sealed trait ISCONST[T <: Boolean]
type CONST = ISCONST[true]

trait DFTypeAny
trait DFBits[W <: Int] extends DFTypeAny
trait DFVal[+T <: DFTypeAny, +M <: ModifierAny]
type DFValAny = DFVal[DFTypeAny, ModifierAny]
type DFValTP[+T <: DFTypeAny, +P] = DFVal[T, Modifier[Any, P]]
type DFConstOf[+T <: DFTypeAny] = DFVal[T, Modifier[Any, CONST]]

trait Candidate[R]:
  type OutW <: Int
  type OutP
object Candidate:
  given [W <: Int, P, R <: DFValTP[DFBits[W], P]]: Candidate[R] with
    type OutW = W
    type OutP = P

extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
  def ^^^[R](rhs: R)(using
    icR: Candidate[R]
  ): DFValTP[DFBits[icL.OutW], icL.OutP | icR.OutP] = ???
  def ^^^ : Unit = ???
extension [L](lhs: L)
  def ^^^[RW <: Int, RP](
    rhs: DFValTP[DFBits[RW], RP]
  )(using es: Summon[L, lhs.type])(using
    c: Candidate[L]
  )(using check: c.OutW =:= c.OutW): DFValTP[DFBits[c.OutW], c.OutP | RP] = ???

val x: DFConstOf[DFBits[8]] = ???
val zzz = x ^^^ x ^^^ x


object Minimized:
  trait DFVal[+T <: Int, +P]

  trait Summon[R, T <: R]
  given [R, T <: R]: Summon[R, T] with {}

  trait Candidate[R]:
    type OutW <: Int
    type OutP
  given [W <: Int, P, R <: DFVal[W, P]]: Candidate[R] with
    type OutW = W
    type OutP = P

  extension [L <: DFVal[Int, Any]](lhs: L)(using icL: Candidate[L])
    def ^^^[R](rhs: R)
              (using icR: Candidate[R])
              : DFVal[icL.OutW, icL.OutP | icR.OutP] = ???
    def ^^^ : Unit = ???

  extension [L](lhs: L)
    def ^^^[RW <: Int, RP](rhs: DFVal[RW, RP])
                          (using es: Summon[L, lhs.type])
                          (using c: Candidate[L])
                          (using check: c.OutW =:= c.OutW)
                          : DFVal[c.OutW, c.OutP | RP] = ???

  val x: DFVal[8, true] = ???
  val z1 = x ^^^ x // Ok
  val z2 = z1 ^^^ x // Ok
  val zzz = x ^^^ x ^^^ x // Error before changes

  /* Before the changes, when `def ^^^ : Unit = ???` is present,
   * all of z1, z2, zzz attempt to use the last `def ^^^`,
   * despite it being less specific than the 1st one.
   */
end Minimized
