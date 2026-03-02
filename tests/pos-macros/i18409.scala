//> using options -Werror -Wunused:imports

import scala.quoted.*

object model {
  trait Transformer[Source, Dest] {
    def transform(from: Source): Dest
  }
  object Transformer {
    trait ForProduct[A, B] extends Transformer[A, B]
  }
}

object Ops {
  import model.Transformer // unused import false-positive

  def unapply(using Quotes)(term: quotes.reflect.Term): Option[String] = {
    term.asExpr match {
      case '{
            ($transformer: Transformer.ForProduct[a, b]).transform($appliedTo)
          } =>
        Some("")
      case other => None
    }
  }
}
