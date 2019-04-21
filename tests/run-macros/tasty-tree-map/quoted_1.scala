import scala.quoted._
import scala.tasty._

object Macros {

  implicit inline def identityMaped[T](x: => T): T = ${ impl('x) }

  def impl[T: Type](x: Expr[T])(implicit reflection: Reflection): Expr[T] = {
    import reflection._
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).seal.cast[T]
    transformed
  }

}
