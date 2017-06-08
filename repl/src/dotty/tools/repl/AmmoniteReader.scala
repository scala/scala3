package dotty.tools
package repl

import terminal.{ Exit => _, _ }
import terminal.filters._
import GUILikeFilters._
import LazyList._

import dotc.printing.SyntaxHighlighting
import dotc.interactive.InteractiveDriver

import java.io.{ OutputStreamWriter, InputStreamReader }

private[repl] class AmmoniteReader(interactive: InteractiveDriver, compiler: AnyRef, history: List[String]) {
  type History = List[String]

  private[this] val reader = new InputStreamReader(System.in)
  private[this] val writer = new OutputStreamWriter(System.out)

  private[this] val cutPasteFilter  = ReadlineFilters.CutPasteFilter()
  private[this] val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  private[this] val multilineFilter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, d) if (lb == 10 || lb == 13) =>
      val isIncomplete =
        ParseResult.isIncomplete(b.mkString)(interactive.currentCtx)

      if (isIncomplete) BasicFilters.injectNewLine(b, c, rest, indent = 2)
      else Result(b.mkString) // short-circuit the filters
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
        (ParseResult(source)(interactive.currentCtx), source :: history)
      }
      .getOrElse((Quit, history))
  }
}
