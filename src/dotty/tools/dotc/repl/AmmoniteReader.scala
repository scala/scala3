package dotty.tools
package dotc
package repl

import core.Contexts._
import ammonite.terminal._
import LazyList._
import Ansi.Color
import filters._
import BasicFilters._
import GUILikeFilters._
import util.SourceFile
import printing.SyntaxHighlighting

class AmmoniteReader(val interpreter: Interpreter)(implicit ctx: Context) extends InteractiveReader {
  val interactive = true

  def incompleteInput(str: String): Boolean =
    interpreter.delayOutputDuring(interpreter.interpret(str)) match {
      case Interpreter.Incomplete => true
      case _ => false
    }

  val reader = new java.io.InputStreamReader(System.in)
  val writer = new java.io.OutputStreamWriter(System.out)
  val cutPasteFilter = ReadlineFilters.CutPasteFilter()
  var history = List.empty[String]
  val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  val multilineFilter: Filter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, _)
      if (lb == 10 || lb == 13) && incompleteInput(b.mkString) =>
      BasicFilters.injectNewLine(b, c, rest)
  }

  def readLine(prompt: String): String = {
    val historyFilter = new HistoryFilter(
      () => history.toVector,
      Console.BLUE,
      AnsiNav.resetForegroundColor
    )

    val allFilters = Filter.merge(
      UndoFilter(),
      historyFilter,
      selectionFilter,
      GUILikeFilters.altFilter,
      GUILikeFilters.fnFilter,
      ReadlineFilters.navFilter,
      cutPasteFilter,
      multilineFilter,
      BasicFilters.all
    )

    Terminal.readLine(
      Console.BLUE + prompt + Console.RESET,
      reader,
      writer,
      allFilters,
      displayTransform = (buffer, cursor) => {
        val coloredBuffer =
          if (ctx.useColors) SyntaxHighlighting(buffer)
          else buffer

        val ansiBuffer = Ansi.Str.parse(coloredBuffer.toVector)
        val (newBuffer, cursorOffset) = SelectionFilter.mangleBuffer(
          selectionFilter, ansiBuffer, cursor, Ansi.Reversed.On
        )
        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor,
          Ansi.Color.Green
        )

        (newNewBuffer, cursorOffset)
      }
    ) match {
      case Some(res) =>
        history = res :: history;
        res
      case None => ":q"
    }
  }
}
