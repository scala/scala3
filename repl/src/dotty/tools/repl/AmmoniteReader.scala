package dotty.tools
package repl

//import terminal._
import terminal.{ Exit => _, _ }
import terminal.filters._
import GUILikeFilters._
import LazyList._
import ParseResult._

import dotc.printing.SyntaxHighlighting
import dotc.interactive.InteractiveDriver

import java.io.{ OutputStreamWriter, InputStreamReader }

/** Denotes ctrl-d fired */
sealed case class Exit()

private[repl] class AmmoniteReader(interactive: InteractiveDriver, compiler: AnyRef, history: List[String]) {
  type History = List[String]

  private[this] val reader = new InputStreamReader(System.in)
  private[this] val writer = new OutputStreamWriter(System.out)

  private[this] val cutPasteFilter  = ReadlineFilters.CutPasteFilter()
  private[this] val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  private[this] val multilineFilter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, d)
    if (lb == 10 || lb == 13) && isIncomplete(b.mkString) =>
      BasicFilters.injectNewLine(b, c, rest, indent = 2)
  }

  private[this] var latestParseResult: Trees | SyntaxErrors | Exit = Exit()
  private def isIncomplete(source: String): Boolean =
    replParse(source)(interactive.currentCtx) match {
      case Incomplete =>
        true

      // FIXME: split into two cases workaround for match on `SyntaxErrors |
      //        Trees` not working with backend
      case res: SyntaxErrors =>
        latestParseResult = res; false
      case res: Trees =>
        latestParseResult = res; false
    }

  def prompt(): (Trees | SyntaxErrors | Exit, History) = {
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

    val newHistory =
      Terminal
      .readLine(prompt, reader, writer, allFilters, displayTransform)
      .map(_ :: history)
      .getOrElse(history)

    (latestParseResult, newHistory)
  }
}
