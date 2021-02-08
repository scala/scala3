import language.experimental.namedTypeArguments
trait Tuple
case class TCons[+H, +T <: Tuple](h: H, t: T) extends Tuple
case object TNil extends Tuple

// Type level natural numbers -------------------------------------------------

sealed trait Nat
sealed trait Succ[P <: Nat] extends Nat
sealed trait Zero extends Nat

// Accessor type class to compute the N'th element of an Tuple L --------------

trait At[L <: Tuple, N <: Nat, Out] {
  def apply(l: L): Out
}

object At {
  implicit def caseZero[H, T <: Tuple]: At[H TCons T, Zero, H] =
    new At[H TCons T, Zero, H] {
      def apply(l: H TCons T): H = {
        val (h TCons _) = l
        h
      }
    }

  implicit def caseN[H, T <: Tuple, N <: Nat, O]
    (implicit a: At[T, N, O]): At[H TCons T, Succ[N], O] =
      new At[H TCons T, Succ[N], O] {
        def apply(l: H TCons T): O = {
          val (_ TCons t) = l
          a(t)
        }
      }
}

// An HMap is an Tuple with HEntry elements. We are reusing Tuple for it's nice syntax

final case class HEntry[K, V](value: V)

// Accessor type class to compute the element of type K in a HMap L -----------

trait PhantomGet[K, M <: Tuple, I <: Nat] // extends PhantomAny

object PhantomGet {
  implicit def getHead[K, V, T <: Tuple]
    : PhantomGet[K, HEntry[K, V] TCons T, Zero] = null

  implicit def getTail[K, H, T <: Tuple, I <: Nat]
    (implicit t: PhantomGet[K, T, I])
    : PhantomGet[K, H TCons T, Succ[I]] = null
}

// Syntax ---------------------------------------------------------------------

object syntax {
  object hmap {
    implicit class HmapGet[M <: Tuple](m: M) {
      def get[K, V, I <: Nat](k: K)
        (implicit
          g: PhantomGet[k.type, M, I],
          a: At[M, I, HEntry[k.type, V]]
        ): V = a(m).value
    }

    def --[K, V](key: K, value: V) = HEntry[key.type, V](value)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import syntax.hmap.*

    val map1 =
      TCons(HEntry[K = "name"]("foo"),
      TCons(HEntry[K = "genre"](true),
      TCons(HEntry[K = "moneyz"](123),
      TCons(HEntry[K = "cat"]("bar"),
      (TNil: TNil.type)))))

    assert(map1.get("name") == "foo")
    assert(map1.get("genre") == true)
    assert(map1.get("moneyz") == 123)
    assert(map1.get("cat") == "bar")

    val map2 =
      TCons(--("name"  , "foo"),
      TCons(--("genre" , true),
      TCons(--("moneyz", 123),
      TCons(--("cat"   , "bar"),
      TNil))))

    assert(map2.get("name") == "foo")
    assert(map2.get("genre") == true)
    assert(map2.get("moneyz") == 123)
    assert(map2.get("cat") == "bar")
  }
}
