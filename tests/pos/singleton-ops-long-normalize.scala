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

    // Val with prefixes are correctly sorted.
    object A:
      val m: Long = 4L
    object B:
      val m: Long = 5L
    import A.{m => am}
    summon[B.m.type + am.type + m.type + A.m.type  =:=  m.type + A.m.type + B.m.type + am.type]

    // Works with inline transparent def.
    transparent inline def f[T <: Long & Singleton](t: T) = t + 5L
    val v1: 9L = f(4L)

    // Arguments are normalized.
    val v2: 4L + (10L/2L) = ???
    summon[v2.type =:= 9L]
    summon[(10L/2L) + m.type =:= m.type + 5L]

    // Ints are converted to Longs when passed as arguments to Long operations.
    summon[1L + 2 =:= 1 + 2L]
    summon[1 + 2 =:= 3L]

    // Works with type parameters inference.
    def mult[A <: Long & Singleton, B <: Long & Singleton](a: A, b: B): A * B = (a * b).asInstanceOf[A * B]
    val v4: 2L * m.type = mult(2L, m)
    val v5: 2L * 3L = mult(2L, 3L)
    val v6: 6L = mult(2L, 3L)

    // Works with dependent parameters.
    def mult2(a: Long, b: Long): a.type * b.type = (a * b).asInstanceOf[a.type * b.type]
    val v7: 2L * m.type = mult2(2L, m)
    val v8: 2L * 3L = mult2(2L, 3L)
    val v9: 6L = mult2(2L, 3L)

    // Term references referencing singleton types are dereferenced.
    val v10: Long = ???
    val v11: v10.type + 1L = ???
    summon[v11.type + 1L =:= v10.type + 2L]

    // Skolems referencing singleton types are dereferenced.
    val v12: n.type * m.type * 3L = mult2(mult2(n, m) /* : (?: (n.type * m.type)) */, 3L)

    // Type aliases and bounds are normalized.
    type Fact[N <: Long & Singleton] <: Long = N match
      case 0L => 0L
      case 1L => 1L
      case _ => Fact[N - 1L] * N
    summon[Fact[3L] =:= 6L]
    type Fact2[N <: Long] <: Long = N match
      case 0L => 0L
      case 1L => 1L
      case _ => Fact2[N - 1L] * N
    summon[Fact2[3L] =:= 6L]
    type Fact3[N <: Long] = N match
      case 0L => 0L
      case 1L => 1L
      case _ => Fact3[N - 1L] * N
    summon[Fact3[3L] =:= 6L]
    type MAD[A <: Long , B <: Long, C <: Long] <: A + (B * C)
    // summon[MAD[2L, 3L, 4L] =:= 14L]
    // Does not work yet. Should type applications be reduced like match types?
    type MAD2[A <: Long , B <: Long, C <: Long] = A + (B * C)
    summon[MAD2[2L, 3L, 4L] =:= 14L]

