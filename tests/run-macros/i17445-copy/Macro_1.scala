import scala.quoted.*

object TestMacro:

  inline def identityTreeMap[T](inline body: T): T =
    ${ identityTreeMapImpl[T]('body) }

  def identityTreeMapImpl[T: Type](body: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    val mapped = new TreeMap {}.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExprOf[T]
