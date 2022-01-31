// Generated from singleton-ops-long-normalize.scala by singleton-ops-int-normalize_make.sh
import scala.compiletime.ops.int.*

object Test:
  def test() =
    // Operations with constant arguments are constant-folded.
    summon[3 =:= 2 + 1]
    summon[1 + 2 =:= 3]
    summon[3 * 2 + 1 =:= 4 * 2 - 1]

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
    summon[2 * (m.type + n.type) =:= 2 * m.type + 2 * n.type]
    summon[2 * (m.type + n.type) =:= 2 * (n.type + m.type)]
    summon[(m.type + n.type) * (m.type + n.type) =:=  m.type * m.type + 2 * m.type * n.type + n.type * n.type]

    // Val with prefixes are correctly sorted.
    object A:
      val m: Int = 4
    object B:
      val m: Int = 5
    import A.{m => am}
    summon[B.m.type + am.type + m.type + A.m.type  =:=  m.type + A.m.type + B.m.type + am.type]

    // Works with inline transparent def.
    transparent inline def f[T <: Int & Singleton](t: T) = t + 5
    val v1: 9 = f(4)

    // Arguments are normalized.
    val v2: 4 + (10/2) = ???
    summon[v2.type =:= 9]
    summon[(10/2) + m.type =:= m.type + 5]

    // Works with type parameters inference.
    def mult[A <: Int & Singleton, B <: Int & Singleton](a: A, b: B): A * B = (a * b).asInstanceOf[A * B]
    val v4: 2 * m.type = mult(2, m)
    val v5: 2 * 3 = mult(2, 3)
    val v6: 6 = mult(2, 3)

    // Works with dependent parameters.
    def mult2(a: Int, b: Int): a.type * b.type = (a * b).asInstanceOf[a.type * b.type]
    val v7: 2 * m.type = mult2(2, m)
    val v8: 2 * 3 = mult2(2, 3)
    val v9: 6 = mult2(2, 3)

    // Term references referencing singleton types are dereferenced.
    val v10: Int = ???
    val v11: v10.type + 1 = ???
    summon[v11.type + 1 =:= v10.type + 2]

    // Skolems referencing singleton types are dereferenced.
    val v12: n.type * m.type * 3 = mult2(mult2(n, m) /* : (?: (n.type * m.type)) */, 3)

    // Non-singleton arguments are not grouped nor ordered, but singletons are.
    summon[(3 | 2) + (3 | 2) + m.type + m.type =:= 2 * m.type + (3 | 2) + (3 | 2)]

    // Differences with non-singleton arguments are still normalized to sums.
    summon[1 - Int =:= -1 * Int + 1]

    // Type aliases and bounds are normalized.
    type Fact[N <: Int & Singleton] <: Int = N match
      case 0 => 0
      case 1 => 1
      case _ => Fact[N - 1] * N
    summon[Fact[3] =:= 6]
    type Fact2[N <: Int] <: Int = N match
      case 0 => 0
      case 1 => 1
      case _ => Fact2[N - 1] * N
    summon[Fact2[3] =:= 6]
    type Fact3[N <: Int] = N match
      case 0 => 0
      case 1 => 1
      case _ => Fact3[N - 1] * N
    summon[Fact3[3] =:= 6]
    type MAD[A <: Int , B <: Int, C <: Int] <: A + (B * C)
    // summon[MAD[2, 3, 4] =:= 14]
    // Does not work yet. Should type applications be reduced like match types?
    type MAD2[A <: Int , B <: Int, C <: Int] = A + (B * C)
    summon[MAD2[2, 3, 4] =:= 14]

