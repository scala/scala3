import scala.quoted._
import scala.quoted.autolift.{given _}

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line: LineNumber = ${lineImpl}

  def lineImpl with (qctx: QuoteContext) : Expr[LineNumber] = {
    import qctx.tasty.{_, given _}
    '{new LineNumber(${rootPosition.startLine})}
  }

}
