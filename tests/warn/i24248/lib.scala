
import scala.quoted.*

trait Thing
object Stuff:
  given Thing()

object lib:
  inline def m: Thing = ${ mImpl[Thing] }

  def mImpl[T](using Quotes, Type[T]): Expr[T] =
    import quotes.reflect.*
    val thing = Implicits.search(TypeRepr.of[T]) match
      case iss: ImplicitSearchSuccess => iss.tree.asExprOf[T]
    '{
      val res = $thing
      res
    }
