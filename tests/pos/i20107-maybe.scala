//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

object foo:
	transparent inline def unapply[F](e: F): F? = Ok(e.asInstanceOf[F])

class A:
  def test(x: Int) = x match
    case foo(e) => e
