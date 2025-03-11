//> using options -Wunused:all -Werror

trait Foldable[F[_]]:
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

type Id[A] = A

given foldableId: Foldable[Id] =
  new Foldable[Id]:
    def foldLeft[A, B](fa: Id[A], b: B)(f: (B, A) => B): B = b
