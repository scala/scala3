trait ZIO[-R, +E, +A] { self =>

  final def +++[R1, B, E1 >: E](that: ZIO[R1, E1, B]): ZIO[Either[R, R1], E1, Either[A, B]] =
    for {
      e <- ZIO.environment[Either[R, R1]]
      r <- e.fold(self.map(Left(_)) provide _, that.map(Right(_)) provide _)
    } yield r
    // Found:    (Left[A, Any] | Right[Any, B])(r)
    // Required: Either[A, B]

  def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = ???

  def map[B](f: A => B): ZIO[R, E, B] = ???

  def provide[R](R: R): ZIO[Any, E, A] = ???
}

object ZIO {
  def environment[R]: ZIO[R, Nothing, R] = ???
}