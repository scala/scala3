import scala.quoted._
import scala.quoted.autolift.given

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ${lineImpl('[T])}

  def lineImpl(x: TypeTag[Unit])(given qctx: QuoteContext): Expr[LineNumber] = {
    import qctx.tasty.{_, given}
    '{new LineNumber(${rootPosition.startLine})}
  }

}
