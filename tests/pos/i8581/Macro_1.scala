import scala.quoted.*

case class Position()
object Position {
  implicit inline def here: Position = ${ genPosition }

  private def genPosition(using Quotes): Expr[Position] = {
    val lineNo: Int = quotes.reflect.Position.ofMacroExpansion.startLine
    '{ Position() }
  }
}
