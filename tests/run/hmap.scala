trait Tuple
trait ::[H, T <: Tuple] extends Tuple
case class TupleCons[H, T <: Tuple](h: H, t: T) extends ::[H, T]
final case object TNil extends Tuple

// Type level natural numbers -------------------------------------------------

sealed trait Nat
sealed trait Succ[P <: Nat] extends Nat
sealed trait Zero extends Nat

// Accessor type class to compute the N'th element of an Tuple L --------------

trait At[L <: Tuple, N <: Nat, Out] {
  def apply(l: L): Out
}

object At {
  implicit def caseZero[H, T <: Tuple]: At[H :: T, Zero, H] =
    new At[H :: T, Zero, H] {
      def apply(l: H :: T): H = {
        val TupleCons(h, _) = l
        h
      }
    }

  implicit def caseN[H, T <: Tuple, N <: Nat, O]
    (implicit a: At[T, N, O]): At[H :: T, Succ[N], O] =
      new At[H :: T, Succ[N], O] {
        def apply(l: H :: T): O = {
          val TupleCons(_, t) = l
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
    : PhantomGet[K, HEntry[K, V] :: T, Zero] = null

  implicit def getTail[K, H, T <: Tuple, I <: Nat]
    (implicit t: PhantomGet[K, T, I])
    : PhantomGet[K, H :: T, Succ[I]] = null
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

    implicit class EntryAssoc[K](k: K) {
      def -- [V](value: V): HEntry[K, V] = HEntry(value)
    }

    type --[A, B] = HEntry[A, B]
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    type MapType1 =
      HEntry["name",   String]  ::
      HEntry["genre",  Boolean] ::
      HEntry["moneyz", Int]     ::
      HEntry["cat",    String]  ::
      TNil.type

    // Since
    val map1: MapType1 =
      TupleCons(HEntry[K = "name"]("foo"),
      TupleCons(HEntry[K = "genre"](true),
      TupleCons(HEntry[K = "moneyz"](123),
      TupleCons(HEntry[K = "cat"]("bar"),
      TNil))))

    import syntax.hmap._

    assert(map1.get("name") == "foo")
    assert(map1.get("genre") == true)
    assert(map1.get("moneyz") == 123)
    assert(map1.get("cat") == "bar")

    type MapType2 =
      "name"   -- String  ::
      "genre"  -- Boolean ::
      "moneyz" -- Int     ::
      "cat"    -- String  ::
      TNil.type

    val map2: MapType2 =
      TupleCons("name"   -- "foo",
      TupleCons("genre"  -- true,
      TupleCons("moneyz" -- 123,
      TupleCons("cat"    -- "bar",
      TNil))))

    assert(map2.get("name") == "foo")
    assert(map2.get("genre") == true)
    assert(map2.get("moneyz") == 123)
    assert(map2.get("cat") == "bar")
  }
}
