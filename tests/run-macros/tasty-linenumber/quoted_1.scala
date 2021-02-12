import scala.quoted.*

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ${lineImpl(Type.of[T])}

  def lineImpl(x: Type[Unit])(using Quotes) : Expr[LineNumber] = {
    import quotes.reflect.*
    '{new LineNumber(${Expr(Position.ofMacroExpansion.startLine)})}
  }

}
