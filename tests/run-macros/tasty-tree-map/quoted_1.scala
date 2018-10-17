import scala.quoted._
import scala.tasty._

object Macros {

  implicit inline def identityMaped[T](x: => T): T = ${ impl('x) }

  def impl[T: Type](x: Expr[T])(implicit st: StagingContext): Expr[T] = {
    import st.reflection._
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.unseal).seal.cast[T]
    transformed
  }

}
