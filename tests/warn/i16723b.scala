// LTS specifc, not applicable to Next thanks to #17548 improvements
trait Empty[T]:
  def empty: T

object Empty:
  inline def withOnly[F[_], R, T](f: [t <: T] => F[t] => R): R = f(null.asInstanceOf[F[T]])

  def mkEmpty[T](t: T): Empty[T] = ???

  inline given emptyGenC[A]: Empty[A] =
    mkEmpty(withOnly[Empty, A, A]([a <: A] => (_: Empty[a]).empty)) // warn

@main def Test =
  Empty.emptyGenC[String]
