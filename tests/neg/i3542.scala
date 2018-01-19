object Test {
  trait TC[A]

  implicit def case1[F[_]](implicit t: => TC[F[Any]]): TC[String] = ???
  implicit def case2[G[_]](implicit r: TC[G[Any]]): TC[Int] = ???

  implicitly[TC[Int]] // error: no implicit argument of type TC[Int] found
}
