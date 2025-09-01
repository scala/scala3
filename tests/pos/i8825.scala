sealed trait Nat
case class Succ[N <: Nat](n: N) extends Nat
case object Zero extends Nat
type Zero = Zero.type
type One = Succ[Zero]

sealed trait HList
case class HCons[+H, +T <: HList](head: H, tail: T) extends HList
case object HNil extends HList
type HNil = HNil.type

trait Length[L <: HList] {
  type Out <: Nat
}
object Length {
  type Aux[L <: HList, Out0 <: Nat] = Length[L] { type Out = Out0 }
  def instance[L <: HList, Out0 <: Nat]: Aux[L, Out0] = new Length[L] { type Out = Out0 }

  given hnilLength: Aux[HNil, Zero] = instance
  given hconsLength: [H, T <: HList] => (length: Length[T]) => Aux[HCons[H, T], Succ[length.Out]] = instance // (*)
  //given hconsLength[H, T <: HList, N <: Nat] (using length: Aux[T, N]): Aux[HCons[H, T], Succ[N]] = instance // (**)
}

val test = summon[Length.Aux[HCons[Int, HNil], One]]