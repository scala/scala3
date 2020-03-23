import scala.quoted.{Expr, QuoteContext}

case class Position()
object Position {
  implicit inline def here: Position = ${ genPosition }

  private def genPosition(implicit qctx: QuoteContext): Expr[Position] = {
    import qctx.tasty.{_, given _}
    // interesting that if we remove the following line => compile successfully
    println(">>>>>>>>>>>>>>>")
    println(rootPosition)
    println()
    println()
    val lineNo: Int = rootPosition.startLine
    println(">>>>>>>>>>>>>>>")
    println(rootPosition)
    println(lineNo)
    println()
    println()
    '{ Position() }
  }
}
