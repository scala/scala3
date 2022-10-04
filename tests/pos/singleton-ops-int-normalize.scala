// Generated from singleton-ops-long-normalize.scala by singleton-ops-int-normalize_make.sh
import scala.compiletime.ops.int.*

object Test:
  // Operations with constant arguments are constant-folded.
  summon[3 =:= 2 + 1]
  summon[1 + 2 =:= 3]

  // Non-constant arguments are sorted.
  val m: Int = 2
  val n: Int = 3
  summon[1 + m.type =:= 1 + m.type]
  summon[1 + m.type =:= m.type + 1]
  summon[1 + m.type + 1 =:= m.type + 2]
  summon[m.type + n.type =:= n.type + m.type]
  summon[m.type * n.type =:= n.type * m.type]
  summon[2 * m.type * n.type =:= 2 * n.type * m.type]

  // -x is normalized to -1 * x
  summon[m.type - n.type =:= -1 * n.type + m.type]

  // Summing x n times is normalized to x * n.
  summon[2 * m.type =:= m.type + m.type]
  summon[2 * m.type + 2 * m.type =:= m.type + 3 * m.type]
  summon[2 * m.type * m.type =:= m.type * 2 * m.type]

  // Addition is distributed over multiplication.
  //summon[2 * (m.type + n.type) =:= 2 * m.type + 2 * n.type]
  //summon[(m.type + n.type) * (m.type + n.type) =:=  m.type * m.type + 2 * m.type * n.type + n.type * n.type]

  // Works with TermRefs arguments referencing other TermRefs.
  type SInt = Int & Singleton
  final val x: Int = ???
  final val y: x.type + 1 = ???
  summon[y.type + 1 =:= x.type + 2]

  // Terms are canceled
  summon[1 + m.type -1 =:= m.type]
  summon[1 + m.type - 1 =:= m.type]
  summon[1 + m.type + Negate[1] =:= m.type]
  summon[1 + m.type - m.type =:= 1]
  summon[1 + m.type + Negate[m.type] =:= 1]

  // Val with prefixes are correctly sorted.
  object A:
    val m: Int = 4
  object B:
    val m: Int = 5
  import A.{m => am}
  summon[B.m.type + am.type + m.type + A.m.type  =:=  m.type + A.m.type + B.m.type + am.type]

  // Works with inline transparent def.
  transparent inline def f[T <: Int & Singleton](t: T) = t + 5
  val a: 9 = f(4)

  // Arguments are normalized.
  val b: 4 + (10/2) = ???
  summon[b.type =:= 9]
  val d: Singleton & Int = ???
  summon[(10/2) + d.type =:= d.type + 5]

  // Non-singleton types are also sorted.
  type Pos <: Int
  type Neg <: Int
  summon[Pos + Neg =:= Neg + Pos]
  // (But not grouped; see tests/neg/singleton-ops-int-normalize.scala)

  // Non-singleton terms do not prevent singleton ones from being grouped.
  def g1[T <: Int](x: T): 1 + T - 1 = x
  def g2[T <: Int](x: T): x.type + T - x.type = x
  def g3[T <: Int](x: T): T - 0 = x
