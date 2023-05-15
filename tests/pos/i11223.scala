object T:
  trait M[F[_]]
  trait EE[A, B]
  trait AE[A]
  trait DE[A]
  trait CE[A]

  type A1 = M[AE]
  type D1 = M[DE] | M[[a] =>> EE[Int, a]]
  type C1 = M[CE]

  trait F[+R, +A]:
    def <+>[U, B](b: F[U, B]): F[R | U, A] = null
    def int: F[R | A1, Int]

  def d1[A](f: => A): F[D1, A] = null
  def m[R, A](f: F[R | C1, A]): F[R | C1, A] = null

  def x = m { // adding type annotation here helps (m[D1 | A1 | C1, Int])
    d1(123).int <+> null
  }
