import scala.util.boundary
import runtime.suspend

trait Monad[F[_]]:

  /** The unit value for a monad */
  def pure[A](x: A): F[A]

  extension [A](x: F[A])
    /** The fundamental composition operation */
    def flatMap[B](f: A => F[B]): F[B]

    /** The `map` operation can now be defined in terms of `flatMap` */
    def map[B](f: A => B) = x.flatMap(f.andThen(pure))

end Monad

trait CanReflect[M[_]]:
  def reflect[R](mr: M[R]): R

trait Monadic[M[_]: Monad]:

  /**
   * Embedding of pure values into the monad M
   */
  def pure[A](a: A): M[A]

  /**
   * Sequencing of monadic values
   *
   * Implementations are required to implement sequencing in a stack-safe
   * way, that is they either need to implement trampolining on their own
   * or implement `sequence` as a tail recursive function.
   *
   * Actually the type X can be different for every call to f...
   * It is a type aligned sequence, but for simplicity we do not enforce this
   * here.
   */
  def sequence[X, R](init: M[X])(f: X => Either[M[X], M[R]]): M[R]

  /**
   * Helper to summon and use an instance of CanReflect[M]
   */
  def reflect[R](mr: M[R])(using r: CanReflect[M]): R = r.reflect(mr)

  /**
   * Reify a computation into a monadic value
   */
  def reify[R](prog: CanReflect[M] ?=> R): M[R] =
    boundary [M[R]]:
      given CanReflect[M]:
        def reflect[R2](mr: M[R2]): R2 =
          suspend [R2, M[R]] (k => mr.flatMap(k.resume))
      pure(prog)

end Monadic