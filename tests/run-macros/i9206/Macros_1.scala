
import scala.quoted.*

object Inspect {
  inline def inspect[T <: AnyKind]: String = ${ inspectTpe[T] }

  def inspectTpe[T <: AnyKind: Type](using Quotes): Expr[String] = {
    import quotes.reflect.TypeRepr
    val tree = TypeRepr.of[T].typeSymbol.tree
    Expr(tree.show)
  }
}
