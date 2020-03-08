object Test {
  trait A
  trait TestConstructor1 { type F[_ <: A] }
  trait TestConstructor2[D] {
    type F[_ <: D]
    class G[X <: D]
    trait TestConstructor3[E] {
      type G[_ <: D & E]
      class H[X <: D & E]
    }
  }

  val v1: TestConstructor1 => Unit = { f =>
    type P[a <: A] = f.F[a] // OK
  }

  val v2: TestConstructor2[A] => Unit = { f =>
    type P[a <: A] = f.F[a] // Error! Type argument a does not conform to upper bound D
  }

  def f2(f: TestConstructor2[A]): Unit = {
    type P[a <: A] = f.F[a] // Error! Type argument a does not conform to upper bound D
  }

  val v3: (f: TestConstructor2[A]) => (g: f.TestConstructor3[A]) => Unit = ??? // ok
  val v4: (f: TestConstructor2[A]) => (g: f.TestConstructor3[A]) => Unit = {f => ???}
  val v5: (f: TestConstructor2[A]) => (g: f.TestConstructor3[A]) => Unit = {(f: TestConstructor2[A]) => ???}
                                                                                                                                           // }
  def f3(f: TestConstructor2[A], g: f.TestConstructor3[A]): Unit = {
    type P[a <: A] = f.F[a] // Error! Type argument a does not conform to upper bound D
    type Q[a <: A] = g.G[a]
    // type R[a <: A] = (f.F & g.G)[a] // compiler error
    type R[a <: A] = ([X <: A] =>> f.F[X] & g.G[X])[a]
    type S[a <: A] = f.G[a] & g.H[a]
  }
}
