object Test {

  // Checking edge cases that should not compile with kind-polymorphism
  trait X[F[_] <: AnyKind] { def a: Int }
  new X[Int] { }   // error
  new X[Int] { }.a // error
  new X[Either] { } // error
  new X[Either] { }.a // error
  new X[({ type l[X, Y] = X })#l] { } // error

}
