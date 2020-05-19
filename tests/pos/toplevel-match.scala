class A
class B

trait HList
class HCons[A, As <: HList] extends HList
class HNil[A] extends HList

type AtoB[Xs <: HList] <: HList = Xs match
  case HNil[a] => HNil[B]
  case HCons[a, as] => HCons[B, AtoB[as]]

//type A2B[Xs <: Tuple] <: Tuple = Xs match
//  case Unit => Unit
//  case a *: as => B *: A2B[as]
