// https://github.com/scala/scala3/issues/26683
// `IsConst` must stay unevaluated wherever the other `compiletime.ops` do,
// rather than answering `false` for a type that is merely not known yet.
import scala.compiletime.ops.any.IsConst
import scala.compiletime.ops.int.+

// A match type that is stuck for an argument whose bound cannot decide the first
// case: `X <: Int` is neither provable nor refutable for an unbounded `X`.
// Non-recursive, so an application is instantiated into a bare `MatchType`.
type Pick[T] <: Int = T match
  case Int => 1
  case _   => 2

// Recursive, so an application stays an `AppliedType` over a match alias.
type Len[T <: Tuple] <: Int = T match
  case EmptyTuple => 0
  case _ *: t     => 1 + Len[t]

type Probe[T] = IsConst[T] match
  case true  => "const"
  case false => "not-const"

object P:
  // with the arguments known, the match types reduce and `IsConst` answers `true`
  summon[Pick[5] =:= 1]
  summon[Probe[Pick[5]] =:= "const"]
  summon[Len[(1, 2, 3)] =:= 3]
  summon[Probe[Len[(1, 2, 3)]] =:= "const"]

  // with the arguments unknown the match types are stuck, so `IsConst` is
  // undetermined and `Probe` cannot reduce either way
  def stuck1[X]: Unit = summon[Probe[Pick[X]] =:= "not-const"] // error
  def stuck2[X]: Unit = summon[Probe[Pick[X]] =:= "const"] // error
  def stuck3[T <: Tuple]: Unit = summon[Probe[Len[T]] =:= "not-const"] // error
  def stuck4[T <: Tuple]: Unit = summon[Probe[Len[T]] =:= "const"] // error

  // once the scrutinee is known again, reduction resumes
  def known[X <: 5]: Unit = summon[Probe[Pick[X]] =:= "const"]
end P

// The same applies to every other type that is not concrete yet: `IsConst` must
// be undetermined exactly where an operation such as `+` is.
trait AbstractMember:
  type T <: Int
  def op: Unit = summon[(T + 1) =:= 2] // error
  def isConst1: Unit = summon[IsConst[T] =:= false] // error
  def isConst2: Unit = summon[IsConst[T] =:= true] // error

trait AbstractConstructor[F[_ <: Int] <: Int]:
  def op[X <: Int]: Unit = summon[(F[X] + 1) =:= 2] // error
  def isConst1[X <: Int]: Unit = summon[IsConst[F[X]] =:= false] // error
  def isConst2[X <: Int]: Unit = summon[IsConst[F[X]] =:= true] // error

// Concrete types are still decided, and an abstract member becomes decidable
// again as soon as a prefix pins it down.
object Concrete:
  summon[IsConst[Int] =:= false]
  summon[IsConst[Any] =:= false]
  summon[IsConst[List[Int]] =:= false]
  def poly[X]: Unit = summon[IsConst[List[X]] =:= false]
  val pinned: AbstractMember { type T = 5 } = ???
  summon[IsConst[pinned.T] =:= true]
end Concrete
