import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    val tree = x.unseal
    '{
      println()
      println("tree: " + ${tree.showExtractors})
      println("tree deref. vals: " + ${tree.underlying.showExtractors})
    }
  }
}
