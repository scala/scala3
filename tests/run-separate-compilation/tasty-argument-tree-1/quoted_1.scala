import scala.quoted._
import scala.tasty._

object Macros {

  inline def inspect[T](x: T): Unit = ~impl('(x))

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._
    val tree = x.reflect
    '{
      println()
      println("tree: " + ~tree.show.toExpr)
      println("tree deref. vals: " + ~tree.underlying.show.toExpr)
    }
  }
}
