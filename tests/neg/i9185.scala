trait M[F[_]] { def pure[A](x: A): F[A] }
object M {
  extension [A, F[A]](x: A) def pure(using m: M[F]): F[A] = m.pure(x)
  given listMonad: M[List] { def pure[A](x: A): List[A] = List(x) }
  given optionMonad: M[Option] { def pure[A](x: A): Option[A] = Some(x) }
  val value1: List[String] = "ola".pure
  val value2 = "ola".pure  // error
  val value3 = M.pure("ola") // error

  extension (x: Int) def len: Int = x
  val l = "abc".len  // error
}