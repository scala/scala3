trait Higher[F[_]]
trait Super[A]
trait Sub[A] extends Super[A]

object Test {
  implicit def higherSub: Higher[Sub] = ???
  implicit def deriv[F[_]](implicit bla: Higher[F]): F[String] = ???

  val x: Super[String] = deriv
}
