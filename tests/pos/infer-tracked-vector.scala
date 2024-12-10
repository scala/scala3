import scala.language.experimental.tracked
import scala.language.future

object typeparams:
  sealed trait Nat
  object Z extends Nat
  final case class S[N <: Nat]() extends Nat

  type Zero = Z.type
  type Succ[N <: Nat] = S[N]

  sealed trait Fin[N <: Nat]
  case class FZero[N <: Nat]() extends Fin[Succ[N]]
  case class FSucc[N <: Nat](pred: Fin[N]) extends Fin[Succ[N]]

  object Fin:
    def zero[N <: Nat]: Fin[Succ[N]] = FZero()
    def succ[N <: Nat](i: Fin[N]): Fin[Succ[N]] = FSucc(i)

  sealed trait Vec[A, N <: Nat]
  case class VNil[A]() extends Vec[A, Zero]
  case class VCons[A, N <: Nat](head: A, tail: Vec[A, N]) extends Vec[A, Succ[N]]

  object Vec:
    def empty[A]: Vec[A, Zero] = VNil()
    def cons[A, N <: Nat](head: A, tail: Vec[A, N]): Vec[A, Succ[N]] = VCons(head, tail)

    def get[A, N <: Nat](v: Vec[A, N], index: Fin[N]): A = (v, index) match
      case (VCons(h, _), FZero()) => h
      case (VCons(_, t), FSucc(pred)) => get(t, pred)

  def runVec(): Unit =
    val v: Vec[Int, Succ[Succ[Succ[Zero]]]] = Vec.cons(1, Vec.cons(2, Vec.cons(3, Vec.empty)))

    println(s"Element at index 0: ${Vec.get(v, Fin.zero)}")
    println(s"Element at index 1: ${Vec.get(v, Fin.succ(Fin.zero))}")
    println(s"Element at index 2: ${Vec.get(v, Fin.succ(Fin.succ(Fin.zero)))}")
    // println(s"Element at index 2: ${Vec.get(v, Fin.succ(Fin.succ(Fin.succ(Fin.zero))))}") // error

// TODO(kπ) check if I can get it to work
// object typemembers:
//   sealed trait Nat
//   object Z extends Nat
//   case class S() extends Nat:
//     type N <: Nat

//   type Zero = Z.type
//   type Succ[N1 <: Nat] = S { type N = N1 }

//   sealed trait Fin:
//     type N <: Nat
//   case class FZero[N1 <: Nat]() extends Fin:
//     type N = Succ[N1]
//   case class FSucc(tracked val pred: Fin) extends Fin:
//     type N = Succ[pred.N]

//   object Fin:
//     def zero[N1 <: Nat]: Fin { type N = Succ[N1] } = FZero[N1]()
//     def succ[N1 <: Nat](i: Fin { type N = N1 }): Fin { type N = Succ[N1] } = FSucc(i)

//   sealed trait Vec[A]:
//     type N <: Nat
//   case class VNil[A]() extends Vec[A]:
//     type N = Zero
//   case class VCons[A](head: A, tracked val tail: Vec[A]) extends Vec[A]:
//     type N = Succ[tail.N]

//   object Vec:
//     def empty[A]: Vec[A] = VNil()
//     def cons[A](head: A, tail: Vec[A]): Vec[A] = VCons(head, tail)

//     def get[A](v: Vec[A], index: Fin { type N = v.N }): A = (v, index) match
//       case (VCons(h, _), FZero()) => h
//       case (VCons(_, t), FSucc(pred)) => get(t, pred)

//   // def runVec(): Unit =
//     val v: Vec[Int] = Vec.cons(1, Vec.cons(2, Vec.cons(3, Vec.empty)))

//     println(s"Element at index 0: ${Vec.get(v, Fin.zero)}")
//     println(s"Element at index 1: ${Vec.get(v, Fin.succ(Fin.zero))}")
//     println(s"Element at index 2: ${Vec.get(v, Fin.succ(Fin.succ(Fin.zero)))}")
//     // println(s"Element at index 2: ${Vec.get(v, Fin.succ(Fin.succ(Fin.succ(Fin.zero))))}")
