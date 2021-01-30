import scala.quoted.*

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T])(using q: Quotes) : Expr[Unit] = {
    import q.reflect.*
    val tree = x.asTerm
    given Printer[Tree] = Printer.TreeStructure
    '{
      println()
      println("tree: " + ${Expr(tree.show)})
      println("tree deref. vals: " + ${Expr(tree.underlying.show)})
    }
  }
}
