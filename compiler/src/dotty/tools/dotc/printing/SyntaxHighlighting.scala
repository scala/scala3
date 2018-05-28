package dotty.tools
package dotc
package printing

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens._
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer
import dotty.tools.dotc.util.Positions.Position

import util.SourceFile

import scala.collection.mutable

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {

  // Keep in sync with SyntaxHighlightingTests
  val NoColor         = Console.RESET
  val CommentColor    = Console.BLUE
  val KeywordColor    = Console.YELLOW
  val ValDefColor     = Console.CYAN
  val LiteralColor    = Console.RED
  val StringColor     = Console.GREEN
  val TypeColor       = Console.MAGENTA
  val AnnotationColor = Console.MAGENTA

  private class NoReporter extends Reporter {
    override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = ()
  }

  def highlight(in: String)(ctx0: Context): String = {
    import dotty.tools.dotc.ast.untpd._

    implicit val ctx: Context = ctx0.fresh.setReporter(new NoReporter)

    val source = new SourceFile("<highlighting>", in.toCharArray)
    val colorAt = Array.fill(in.length)(NoColor)

    def highlightRange(from: Int, to: Int, color: String) = {
      try {
        for (i <- from until to)
          colorAt(i) = color
      } catch {
        case _: IndexOutOfBoundsException =>
          println("Encountered tree with invalid position, please open an issue with the code snippet that caused the error")
      }
    }
    def highlightPosition(pos: Position, color: String) =
      if (pos.exists) highlightRange(pos.start, pos.end, color)

    val scanner = new Scanner(source)

    while (scanner.token != EOF) {
      val isKwd = alphaKeywords.contains(scanner.token)
      val offsetStart = scanner.offset

      if (scanner.token == IDENTIFIER && scanner.name == nme.???) {
        highlightRange(scanner.offset, scanner.offset + scanner.name.length, Console.RED_B)
      }
      scanner.nextToken()

      if (isKwd) {
        val offsetEnd = scanner.lastOffset
        highlightPosition(Position(offsetStart, offsetEnd), KeywordColor)
      }
    }

    val treeHighlighter = new UntypedTreeTraverser {
      def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case id : Ident if id.isType =>
            highlightPosition(id.pos, TypeColor)
          case tpe : TypeDef =>
            for (annotation <- tpe.rawMods.annotations)
              highlightPosition(annotation.pos, AnnotationColor)
            highlightPosition(tpe.namePos, TypeColor)
          case _ : TypTree =>
            highlightPosition(tree.pos, TypeColor)
          case mod: ModuleDef =>
            highlightPosition(mod.namePos, TypeColor)
          case v : ValOrDefDef =>
            for (annotation <- v.rawMods.annotations)
              highlightPosition(annotation.pos, AnnotationColor)
            highlightPosition(v.namePos, ValDefColor)
            highlightPosition(v.tpt.pos, TypeColor)
          case _ : Literal =>
            highlightPosition(tree.pos, LiteralColor)
          case _ =>
        }
        traverseChildren(tree)
      }
    }

    val parser = new Parser(source)
    val trees = parser.blockStatSeq()

    for (tree <- trees)
      treeHighlighter.traverse(tree)

    val sb = new mutable.StringBuilder()

    for (idx <- colorAt.indices) {
      if ( (idx == 0 && colorAt(idx) != NoColor)
        || (idx > 0 && colorAt(idx-1) != colorAt(idx))) {
        sb.append(colorAt(idx))
      }
      sb.append(in(idx))
    }
    if (colorAt.nonEmpty && colorAt.last != NoColor) {
      sb.append(NoColor)
    }

    sb.toString
  }
}
