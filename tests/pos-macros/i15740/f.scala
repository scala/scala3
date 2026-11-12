package f

type R
type I >: R

sealed trait F[+R, +A]
object F {
  def p[A](x: A): F[Nothing, A] = ???

  def r[E : T](e: E): F[R, Nothing] = ???

  /* this method is never used, but affects compilation
   * the code compiles without it!
   */
  def unused[A](f: A): F[I, A] = F.r(1)
}

val _ = (x: F[I, Unit]) // used to fail: Found F[I, Unit], Required F[I², Unit]
