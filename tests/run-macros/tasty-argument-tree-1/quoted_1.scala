import scala.quoted._

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](using s: Scope)(x: s.Expr[T]): s.Expr[Unit] = {
    import s.tasty._
    val tree = x
    '{
      println()
      println("tree: " + ${Expr(tree.showExtractors)})
      println("tree deref. vals: " + ${Expr(tree.underlying.showExtractors)})
    }
  }
}
