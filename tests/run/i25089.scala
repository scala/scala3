def runAsRight[T](execute: => T): Either[String, T] = Right(execute)

@main def Test =
  for
    _ <- runAsRight(
      println("Printed for both versions"),
    )
    _ = println(s"Printed only with 3.7.4")
  yield ()
