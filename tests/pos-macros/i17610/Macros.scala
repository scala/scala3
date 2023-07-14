// Macros.scala
import Main._
import scala.quoted.*

object Macros {
  inline def apply(): ProviderProcessor =
      ${ Macros.processorExpr }

  def processorExpr[I: Type](using q: Quotes): Expr[ProviderProcessor] = '{
    new ProviderProcessor {
      override def apply(simple: Simple): MyF[Int] =
        ${ Macros.methodProcessorImpl('simple) }
    }
  }

  def methodProcessorImpl[I: Type](using q: Quotes)(service: Expr[Simple]): Expr[MyF[Int]] = {
    import q.reflect._

    val returnType = TypeRepr.of[Int]
    returnType.asType match {
      case '[rt] =>
        '{
          ${
            import quotes.reflect._
            TypeApply(
              Select.unique('{ ???.asInstanceOf[Codec] }.asTerm, "apply"),
              List(TypeTree.of[rt]) // generates the error, directly using Int instead of rt makes it disappear
            ).asExpr
          }
          ???
        }
    }
  }
}
