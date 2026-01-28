import scala.quoted.*

object Inner:
  inline def pos(inline x: Any): Unit =
    ${ posImpl('x) }

  def posImpl(x: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    def printPos(p: Position): Expr[Unit] =
      val str = Expr(s"${p.start}..${p.end} of ${p.sourceFile.path} with length ${p.sourceFile.content.get.length}")
      '{println($str)}
    '{
      ${printPos(x.asTerm.pos)}
      ${printPos(x.asTerm.asInstanceOf[Inlined].body.pos)}
      ${printPos(x.asTerm.asInstanceOf[Inlined].body.asInstanceOf[Inlined].body.pos)}
    }
