object Test {
  trait A
  trait B
  trait TestConstructor1 { type F[X <: A]
    trait FromSet[C[_ <: A with B]]

    trait MSetLike[X <: A with B, This <: MSet[X] with MSetLike[X, This]] {
      def to[C[X <: A with B] <: MSet[X] with MSetLike[X, C[X]]](fi: FromSet[C]): C[X] = ???
    }
    trait MSet[X <: A with B] extends MSetLike[X, MSet[X]]
    object MSetFactory extends FromSet[MSet]
  }

  trait TestConstructor4[D] {
    trait TestConstructor5[E] {
      trait FromSet[C[_ <: D with E]]

      trait MSetLike[X <: D with E, This <: MSet[X] with MSetLike[X, This]] {
        def to[C[X <: D with E] <: MSet[X] with MSetLike[X, C[X]]](fi: FromSet[C]): C[X] = ???
      }
      trait MSet[X <: D with E] extends MSetLike[X, MSet[X]]
      object MSetFactory extends FromSet[MSet]
    }
  }

  type C = A & B
  val v1: TestConstructor1 => Unit = { f =>
    type P[a <: A] = f.F[a]

    type P1[c <: C] = f.MSet[c]
      (f.MSetFactory: f.FromSet[f.MSet]): Unit
      (x: P1[C]) => x.to(f.MSetFactory)
  }

  def f3(f: TestConstructor4[A])(g: f.TestConstructor5[B]): Unit = {
    type P1[c <: C] = g.MSet[c]
      (g.MSetFactory: g.FromSet[g.MSet]): Unit
      (x: P1[C]) => x.to(g.MSetFactory)
      (x: P1[C]) => x.to[g.MSet](g.MSetFactory)
  }
}