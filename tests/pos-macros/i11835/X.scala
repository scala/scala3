import scala.quoted.*

object X:
  inline def blah(inline b: Boolean = true): Unit =
    ${ _blah('b) }

  private def _blah(b: Expr[Boolean])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    println("="*120)
    println(b.asTerm)
    println(b.valueOrError)
    '{()}
