
class Container[Y1, G[_]]:
  lazy val outer: Knit[CP, G] = new:
    type Res = Y1
    def visit[R](caseInFst: [F1[_], Y] => (k: Knit[CP, F1]) => (ev: TypeEqK[G, [x] =>> CP[F1[x], Y]]) => R): R =
      caseInFst[G, Res](outer)(new TypeEqK[G, [x] =>> CP[G[x], Res]] {})

@main def Test =
  val knit = new Container[Unit, Option].outer
  val res = knit.visit:
    [F1[_], Y] => (k: Knit[CP, F1]) => (ev: TypeEqK[Option, [x] =>> CP[F1[x], Y]]) => 42
  assert(res == 42)
