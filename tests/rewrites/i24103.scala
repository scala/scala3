abstract class IndexedTraversal_[I, S, T, A, B]:
  final def overF[F[_]](f: ((A, I)) => F[B])(s: S): F[T] = ???

type Id[A] = A

trait IndexedTraversalLaws[I, S, A] {
  def indexedTraversal: IndexedTraversal_[I, S, S, A, A]

  def consistentFoci(s: S, f: (A, I) => A, g: (A, I) => A) =
    (indexedTraversal.overF[Id](f.tupled) _ compose indexedTraversal.overF[Id](g.tupled))(s)
    ???
}
