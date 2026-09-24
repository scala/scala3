//> using options -Werror -Wunused:all
import scala.quoted.*

object test {
  import scala.concurrent.duration.FiniteDuration

  def applyImpl[A: Type](using Quotes): Expr[Unit] =
    Type.of[A] match {
      case '[type d <: FiniteDuration; d] => '{ () }
      case _ => '{ () }
    }
}
