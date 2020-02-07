object Example extends App {

  def assertLazy[A, B](f: (A) => B): Boolean = ???

  def fromEither[E, F](eea: Either[E, F]): Unit = ???

  lazy val result = assertLazy(fromEither)

  println("It compiles!")
}