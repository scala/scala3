import scala.quoted.*

def f[T](using t: Type[T])(using Quotes) =
  '{
    // @SplicedType type t$1 = t.Underlying
    type T2 = T // type T2 = t$1
    ${

      val t0: T = ???
      val t1: T2 = ??? //  val t1: T = ???
      val tp1 = Type.of[T] // val tp1 = t
      val tp2 = Type.of[T2] //  val tp2 = t
      '{
        // @SplicedType type t$2 = t.Underlying
        val t3: T = ??? // val t3: t$2 = ???
        val t4: T2 = ???  // val t4: t$2 = ???
      }
    }
  }

def g(using Quotes) =
  '{
    type U
    type U2 = U
    ${

      val u1: U = ???
      val u2: U2 = ??? //  val u2: U = ???

      val tp1 = Type.of[U] // val tp1 = Type.of[U]
      val tp2 = Type.of[U2]  // val tp2 = Type.of[U]
      '{
        val u3: U = ???
        val u4: U2 = ??? // val u4: U = ???
      }
    }
  }
