type R[F[_], A] =[B] => (A => F[B]) => F[B]

type M[F[_]] =[A, B] => (A => F[B]) => F[A] => F[B]

def mr[F[_]]: M[[A] =>> R[F, A]] =
   [A, B] => (f: A => R[F, B]) => (m: R[F, A]) =>
      [C] => (k: B => F[C]) => m(a => f(a)(k))