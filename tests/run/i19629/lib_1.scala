trait CP[A,B]
trait TypeEqK[F[_], G[_]]

trait Knit[CP[_, _], F[_]] {
  type Res

  def visit[R](
    caseInFst: [F1[_], Y] => (k: Knit[CP, F1]) => (ev: TypeEqK[F, [x] =>> CP[F1[x], Y]]) => R
  ): R
}
