given foo(using x: Int): AnyRef with
  type T = x.type

// #7859

trait Lub2[A, B]:
  type Output

given [A <: C, B <: C, C]: Lub2[A, B] with
  type Output = C

trait Lub[Union]:
  type Output

given [A]: Lub[A] with
  type Output = A

given [Left, Right](
    using lubLeft: Lub[Right], lubRight: Lub[Right])(
    using lub2: Lub2[lubLeft.Output, lubRight.Output])
  : Lub[Left | Right] with
  type Output = lub2.Output
