class C[T] {
  type F[_]; type R
}

type X[T] = (C[T] { type F[_]; type R = F[T]})#R  // OK

type Y[T] = { type F[_]; type R = F[T]} // error // error (2 feature warnings)
type Z[T] = ({ type F[_]; type R = F[T]})#R // error // error // error (2 feature warnings + 1 recursion limit exceeded)
