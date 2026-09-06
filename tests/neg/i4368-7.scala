object Test7 {
  class Fix[F[_]] {
    class Foo { type R >: F[T] <: F[T] } // error
    type T = F[Foo#R]
  }

  object App {
    type Nat = Fix[Option]#T
  }
}
