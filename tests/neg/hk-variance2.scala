
trait S {
  type F[+X]
}
trait T {
  type F[-X]
}
object Test2 {
  object O extends S, T {
    type F[X] = Int  // OK
  }
  object O2 extends S, T {
    type F[X] = X  // error
  }
  object O3 extends S, T {
    type F[X] = X => X // error
  }
}
object Test3 {
  object O extends S, T {
    type F = [X] =>> Int  // OK
  }
  object O2 extends S, T {
    type F = [X] =>> X  // error
  }
  object O3 extends S, T {
    type F = [X] =>> X => X // error
  }
}
