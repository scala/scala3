object App {
  type T[G[X] <: X, F[X] <: G[F[X]]] // error
}
