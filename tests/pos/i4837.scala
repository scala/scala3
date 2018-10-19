trait T[X: Numeric]

class C {
  type S[X] = T[X]
  type Foo[X, Y] = T[X]
  type Bar[X, Y] = Foo[X, Y]

  class D[X: Numeric] extends S[X]
  class E[X: Numeric] extends Foo[X, Unit]
  class F[X: Numeric] extends Bar[X, Unit]
}
