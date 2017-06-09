package dotty.tools
package repl

import java.io.{ OutputStreamWriter, InputStreamReader }

import dotc.core.Contexts.Context
import dotc.printing.SyntaxHighlighting
import dotc.interactive.InteractiveDriver

import terminal._
import terminal.filters._
import GUILikeFilters._
import LazyList._
import AmmoniteReader._

private[repl] class AmmoniteReader(history: History)(implicit ctx: Context) {

  private[this] val reader = new InputStreamReader(System.in)
  private[this] val writer = new OutputStreamWriter(System.out)

  private[this] val cutPasteFilter  = ReadlineFilters.CutPasteFilter()
  private[this] val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  private[this] val multilineFilter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, d) if (lb == 10 || lb == 13) =>
      val source = b.mkString

      if (ParseResult.isIncomplete(source))
        BasicFilters.injectNewLine(b, c, rest, indent = 2)
      else
        Result(source) // short-circuit the filters
  }

  def prompt(): (ParseResult, History) = {
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

    def displayTransform(buffer: Vector[Char], cursor: Int): (Ansi.Str, Int) = {
      val coloredBuffer =
        SyntaxHighlighting(buffer)

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

    val prompt = Console.BLUE + "scala> " + Console.RESET

    Terminal
      .readLine(prompt, reader, writer, allFilters, displayTransform)
      .map { source =>
        (ParseResult(source), source :: history)
      }
      .getOrElse((Quit, history))
  }
}

object AmmoniteReader {
  type History = List[String]

  def apply(history: History)(implicit ctx: Context) =
    new AmmoniteReader(history)
}
