import scala.compiletime.ops.long.*

object Test:
  def test() =
    // Operations with constant arguments are constant-folded.
    summon[3L =:= 2L + 1L]
    summon[1L + 2L =:= 3L]
    summon[3L * 2L + 1L =:= 4L * 2L - 1L]

    // Non-constant arguments are sorted.
    val m: Long = 2L
    val n: Long = 3L
    summon[1L + m.type =:= 1L + m.type]
    summon[1L + m.type =:= m.type + 1L]
    summon[1L + m.type + 1L =:= m.type + 2L]
    summon[m.type + n.type =:= n.type + m.type]
    summon[m.type * n.type =:= n.type * m.type]
    summon[2L * m.type * n.type =:= 2L * n.type * m.type]

    // -x is normalized to -1L * x
    summon[m.type - n.type =:= -1L * n.type + m.type]

    // Summing x n times is normalized to x * n.
    summon[2L * m.type =:= m.type + m.type]
    summon[2L * m.type + 2L * m.type =:= m.type + 3L * m.type]
    summon[2L * m.type * m.type =:= m.type * 2L * m.type]

    // Addition is distributed over multiplication.
    summon[2L * (m.type + n.type) =:= 2L * m.type + 2L * n.type]
    summon[2L * (m.type + n.type) =:= 2L * (n.type + m.type)]
    summon[(m.type + n.type) * (m.type + n.type) =:=  m.type * m.type + 2L * m.type * n.type + n.type * n.type]

    // Works with TermRefs arguments referencing other TermRefs.
    type SLong = Long & Singleton
    val x: Long = ???
    val y: x.type + 1L = ???
    summon[y.type + 1L =:= x.type + 2L]

    // Val with prefixes are correctly sorted.
    object A:
      val m: Long = 4L
    object B:
      val m: Long = 5L
    import A.{m => am}
    summon[B.m.type + am.type + m.type + A.m.type  =:=  m.type + A.m.type + B.m.type + am.type]

    // Works with inline transparent def.
    transparent inline def f[T <: Long & Singleton](t: T) = t + 5L
    val a: 9L = f(4L)

    // Arguments are normalized.
    val b: 4L + (10L/2L) = ???
    summon[b.type =:= 9L]
    val d: Singleton & Long = ???
    summon[(10L/2L) + d.type =:= d.type + 5L]

    // Ints are converted to Longs when passed as arguments to Long operations.
    summon[1L + 2 =:= 1 + 2L]
    summon[1 + 2 =:= 3L]

    // Non-singleton arguments are left as-is, but singletons are grouped.
    summon[(3L | 2L) + (3L | 2L) + m.type + m.type =:= 2L * m.type + (3L | 2L) + (3L | 2L)] // error
