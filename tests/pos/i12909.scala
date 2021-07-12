package example

final case class Writer[W, A](run: (W, A)) {
  def map[B](f: A => B): Writer[W, B] = ???

  def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = ???
}

object Main {
  implicit class WriterOps[A](a: A) {
    def set[W](w: W): Writer[W, A] = ???
  }

  def x1[A]: Writer[Vector[String], Option[A]] = ???

  val failure = for {
    a1 <- {
      Option(1) match {
        case Some(x) =>
          x1[Boolean]
        case _ =>
          Option.empty[Boolean].set(Vector.empty[String])
      }
    }
    a2 <- x1[String]
  } yield ()

  val success = for {
    a1 <- {
      val temp = Option(1) match {
        case Some(x) =>
          x1[Boolean]
        case _ =>
          Option.empty[Boolean].set(Vector.empty[String])
      }
      // why ???
      temp
    }
    a2 <- x1[String]
  } yield ()

}