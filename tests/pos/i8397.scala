given foo: (x: Int) => AnyRef:
  type T = x.type

// #7859

trait Lub2[A, B]:
  type Output

given [A <: C, B <: C, C] => Lub2[A, B]:
  type Output = C

trait Lub[Union]:
  type Output

given [A] => Lub[A]:
  type Output = A

given [Left, Right]
    => (lubLeft: Lub[Right], lubRight: Lub[Right])
    => (lub2: Lub2[lubLeft.Output, lubRight.Output])
    => Lub[Left | Right]:
  type Output = lub2.Output
