package dotty.tools.bot
package util

import cats.syntax.applicative._
import scalaz.concurrent.Task

object TaskIsApplicative {
  implicit val taskIsApplicative = new cats.Applicative[Task] {
    def pure[A](x: A): Task[A] = Task.now(x)
    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      for(f <- ff; a <- fa) yield f(a)
  }
}
