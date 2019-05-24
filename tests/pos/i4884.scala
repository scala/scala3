object Test {
  trait A
  trait B
  trait TestConstructor1 { type F[X <: A] <: TestConstructor2[A] }
  trait TestConstructor2[D] {
    type F[_ <: D]
    class G[X <: D]
    trait TestConstructor3[E] {
      type G[X <: D & E] <: TestConstructor2.this.F[X] & H[X]
      class H[X <: D & E] {
        type A <: G[X]
      }
    }
  }
  trait TestConstructor4[D] {
    trait TestConstructor5[E] {
      trait MSetLike[X <: D & E, This <: MSet[X] with MSetLike[X, This]]
      trait MSet[X <: D & E] extends MSetLike[X, MSet[X]]
    }
  }

  val v1: TestConstructor1 => Unit = { f =>
    type P[a <: A] = f.F[a]
  }

  val v2: TestConstructor2[A] => Unit = { f =>
    type P[a <: A] = f.F[a]
  }

  def f2(f: TestConstructor2[A]): Unit = {
    type P[a <: A] = f.F[a]
  }

  type C = A & B
  def f3(f: TestConstructor2[A], g: f.TestConstructor3[B]): Unit = {
    type P1[a <: A] = f.F[a]
    type P2[a <: A] = f.G[a]
    type Q1[c <: C] = g.G[c]
    type Q2[c <: C] = g.H[c]
    type R1[c <: C] = f.G[c] & g.H[c]
    type R2[c <: C] = f.G[c] | g.H[c]
    type S1[c <: C] = ([X <: C] =>> f.F[X] & g.G[X])[c]
    type S2[c <: C] = ([X <: C] =>> f.F[X] | g.G[X])[c]
  }
  def f3(f: TestConstructor4[A], g: f.TestConstructor5[B]): Unit = {
    type P1[c <: C] = g.MSet[c]
  }
}
