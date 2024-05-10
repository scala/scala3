import scala.quoted.*
import scala.compiletime.*

trait Context
object Scope:
  def spawn[A](f: Context ?=> A): A = ???

type Contextual[T] = Context ?=> T

object Macros {
  inline def transformContextLambda[T](inline expr: Context ?=> T): Context => T =
    ${ transformContextLambdaImpl[T]('expr) }

  def transformContextLambdaImpl[T: Type](
      cexpr: Expr[Context ?=> T]
  )(using Quotes): Expr[Context => T] = {
    import quotes.reflect.*
    val tree = asTerm(cexpr)
    val traverse = new TreeMap() {}
    println(tree.show)
    traverse.transformTree(tree)(tree.symbol)
    '{ _ => ??? }
  }
}
