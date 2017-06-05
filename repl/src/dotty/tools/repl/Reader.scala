package dotty.tools
package repl

import terminal._
import terminal.filters._
import LazyList._
import GUILikeFilters._

import dotc.printing.SyntaxHighlighting

import java.io.{ OutputStreamWriter, InputStreamReader }

class AmmoniteReader {
  private[this] val reader = new InputStreamReader(System.in)
  private[this] val writer = new OutputStreamWriter(System.out)

  private[this] var history = List.empty[String]

  private[this] val cutPasteFilter  = ReadlineFilters.CutPasteFilter()
  private[this] val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  private[this] val multilineFilter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, _)
    if (lb == 10 || lb == 13) /*&& incompleteInput(b.mkString)*/ =>
      BasicFilters.injectNewLine(b, c, rest, indent = 2)
  }

  def prompt(): Unit = {
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
      .foreach(res => history = res :: history)
  }
}
