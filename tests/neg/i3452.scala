object Test {
  case class Tuple2K[H[_], T[_], X](h: H[X], t: T[X])

  trait TC[A]

  implicit def case1[F[_]](implicit t: => TC[F[Any]]): TC[Tuple2K[[_] =>> Any, F, Any]] = ???
  implicit def case2[A, F[_]](implicit r: TC[F[Any]]): TC[A] = ???

  implicitly[TC[Int]] // error
}
object Test2 {
  trait TC[A]

  implicit def case1[F[_]](implicit t: => TC[F[Any]]): TC[String] = ???
  implicit def case2[G[_]](implicit r: TC[G[Any]]): TC[Int] = ???

  implicitly[TC[Int]] // error
}
