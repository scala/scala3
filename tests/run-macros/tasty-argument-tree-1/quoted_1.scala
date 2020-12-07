import scala.quoted._

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    val tree = Term.of(x)
    '{
      println()
      println("tree: " + ${Value(tree.showExtractors)})
      println("tree deref. vals: " + ${Value(tree.underlying.showExtractors)})
    }
  }
}
