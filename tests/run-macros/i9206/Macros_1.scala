
import scala.quoted._

object Inspect {
  inline def inspect[T <: AnyKind]: String = ${ inspectTpe[T] }

  def inspectTpe[T <: AnyKind](using s: Scope)(using tpe: s.Type[T]): s.Expr[String] = {
    val tree = summon[s.Type[T]].tpe.typeSymbol.tree
    Expr(tree.show)
  }
}
