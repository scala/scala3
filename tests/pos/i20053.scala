
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

  trait Sub[T, R >: T]
  given [T, R >: T]: Sub[T, R] with {}

  trait Candidate[-R]:
    type OutP
  given [P]: Candidate[Option[P]] with
    type OutP = P

  extension [L <: Option[Any]](lhs: L)(using icL: Candidate[L])
    def ^^^[R](rhs: R)(using icR: Candidate[R]): Option[icL.OutP | icR.OutP] = ???
    def ^^^ : Unit = ???

  extension [L](lhs: L)
    def ^^^[R](rhs: Option[R])
              (using es: Sub[lhs.type, L])
              (using c: Candidate[L])
              (using check: c.OutP =:= c.OutP): Option[c.OutP | R] = ???

  val x: Option[true] = ???
  val z1 = x ^^^ x // Ok
  val z2 = z1 ^^^ x // Ok
  val zzz = x ^^^ x ^^^ x // Error before changes

  /* Before the changes, when `def ^^^ : Unit = ???` is present,
   * all of z1, z2, zzz attempt to use the last `def ^^^`,
   * despite it being less specific than the 1st one.
   */
end Minimized
