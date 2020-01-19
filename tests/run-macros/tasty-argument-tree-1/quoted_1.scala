import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  inline def inspect[T](x: T): Unit = ${ impl('x) }

  def impl[T](x: Expr[T]) with (qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{_, given _}
    val tree = x.unseal
    '{
      println()
      println("tree: " + ${tree.showExtractors})
      println("tree deref. vals: " + ${tree.underlying.showExtractors})
    }
  }
}
