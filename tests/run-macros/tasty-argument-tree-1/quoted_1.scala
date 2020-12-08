import scala.quoted._

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T])(using q: Quotes) : Expr[Unit] = {
    import q.reflect._
    val tree = Term.of(x)
    given Printer[Tree] = Printer.TreeStructure
    '{
      println()
      println("tree: " + ${Expr(tree.show)})
      println("tree deref. vals: " + ${Expr(tree.underlying.show)})
    }
  }
}
