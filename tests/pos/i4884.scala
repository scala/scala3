object Test {
  trait A
  trait TestConstructor1 { type F[_ <: A] }
  trait TestConstructor2[D] { type F[_ <: D] }

  val v1: TestConstructor1 => Unit = { f =>
    type P[a <: A] = f.F[a] // OK
  }

  val v2: TestConstructor2[A] => Unit = { f =>
    type P[a <: A] = f.F[a] // Error! Type argument a does not conform to upper bound D
  }
}