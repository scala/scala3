import scala.quoted.*

trait ReproTransformer[A, B] {
  def transform(from: A): B
}

object ReproTransformer {
  final class Identity[A, B >: A] extends ReproTransformer[A, B] {
    def transform(from: A): B = from
  }

  given identity[A, B >: A]: Identity[A, B] = Identity[A, B]

  inline def getTransformer[A, B]: ReproTransformer[A, B] = ${ getTransformerMacro[A, B] }

  def getTransformerMacro[A, B](using quotes: Quotes, A: Type[A], B: Type[B]) = {
    import quotes.reflect.*

    val transformer = (A -> B) match {
      case '[a] -> '[b] =>
        val summoned = Expr.summon[ReproTransformer[a, b]].get
// ----------- INTERESTING STUFF STARTS HERE
        summoned match {
          case '{ $t: ReproTransformer[src, dest] } => t
        }
// ----------- INTERESTING STUFF ENDS HERE
    }
    transformer.asExprOf[ReproTransformer[A, B]]
  }
}
