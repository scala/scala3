import dotty.{Tuple, TupleCons, LargeTuple}
import dotty.{TupleCons => ::}

// Type level natural numbers ---------------------------------------------------------------------

sealed trait Nat
sealed trait Succ[P <: Nat] extends Nat
sealed trait Zero extends Nat

// Nat ↔ Int type class ---------------------------------------------------------------------------

trait Nat2Int[N <: Nat] {
  type Out <: Singleton & Int
  def value: Out
}

object Nat2Int {
  type Aux[N <: Nat, I <: Singleton & Int] =           Nat2Int[N] { type Out = I }
  def  aux[N <: Nat, I <: Singleton & Int](i: I) = new Nat2Int[N] { type Out = I;  def value: I = i }
  implicit val _0: Aux[Zero, 0] = aux(0)
  implicit val _1: Aux[Succ[Zero], 1] = aux(1)
  implicit val _2: Aux[Succ[Succ[Zero]], 2] = aux(2)
  implicit val _3: Aux[Succ[Succ[Succ[Zero]]], 3] = aux(3)
  implicit val _4: Aux[Succ[Succ[Succ[Succ[Zero]]]], 4] = aux(4)
}

// Accessor type class to compute the N'th element of an Tuple L ----------------------------------

trait At[L <: Tuple, N <: Nat, Out] {
  def apply(l: L): Out
}

object At {
  implicit def lowLevelAt[L <: Tuple, N <: Nat, O]
    (implicit a: PhantomAt[L, N, O], i: Nat2Int[N]): At[L, N, O] =
      new At[L, N, O] {
        def apply(l: L): O = (l match {
          case scala.Tuple1(e1)         => e1
          case scala.Tuple2(e1, e2)     => if (i.value == 0) e1 else e2
          case scala.Tuple3(e1, e2, e3) => if (i.value == 0) e1 else if (i.value == 1) e2 else e3
          case t: LargeTuple[_, _] => t.underlying(i.value)
        }).asInstanceOf[O]
      }
}

trait PhantomAt[L <: Tuple, N <: Nat, Out]
object PhantomAt {
  implicit def caseZero[H, T <: Tuple]: PhantomAt[H :: T, Zero, H] = null
  implicit def caseN[H, T <: Tuple, N <: Nat, O]
    (implicit a: PhantomAt[T, N, O]): PhantomAt[H :: T, Succ[N], O] = null
}

// Syntax -----------------------------------------------------------------------------------------

object syntax {
  object at {
    implicit class TupleAt[L <: Tuple](l: L) {
      def at[N <: Nat, O](i: Int)(implicit n: Nat2Int.Aux[N, i.type], e: At[L, N, O]): O = e(l)
    }
  }

  object nat {
    type _0 = Zero
    type _1 = Succ[Zero]
    type _2 = Succ[Succ[Zero]]
    type _3 = Succ[Succ[Succ[Zero]]]
    type _4 = Succ[Succ[Succ[Succ[Zero]]]]

    // Going from Int to Nat (Option[N] is used as a "type proxy" here)
    def nat[N <: Nat](i: Int)(implicit e: Nat2Int.Aux[N, i.type]): Option[N] = None

    // Going from Nat to Int
    def int[N <: Nat](implicit e: Nat2Int[N]): e.Out = e.value
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import syntax.at._
    import syntax.nat._

    val u: String :: Unit = TupleCons("s", ())

    // assert(u.at(0)  == "s")

    // implicitly[PhantomAt[(String, Int, Int, Int, Boolean), Zero, String]]
    // val tuple: (String, Int, Int, Int, Boolean) = ("s", 1, 2, 3, true)

    // assert(tuple.at(0) == "s")
    // // assert(tuple.at(1) == 1)
    // // assert(tuple.at(2) == 2)
    // // assert(tuple.at(3) == 3)
    // // assert(tuple.at(4) == true)

    // assert(int[_0] == 0)
    // assert(int[_1] == 1)
    // assert(int[_2] == 2)
    // assert(int[_3] == 3)
    // assert(int[_4] == 4)

    // nat(0): Option[_0]
    // nat(1): Option[_1]
    // nat(2): Option[_2]
    // nat(3): Option[_3]
    // nat(4): Option[_4]
  }
}
