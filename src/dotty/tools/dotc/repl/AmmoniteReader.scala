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

class AmmoniteReader extends InteractiveReader {
  val interactive = true

  val reader = new java.io.InputStreamReader(System.in)
  val writer = new java.io.OutputStreamWriter(System.out)
  val cutPasteFilter = ReadlineFilters.CutPasteFilter()
  var history = List.empty[String]
  val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  val multilineFilter: Filter = Filter("multilineFilter") {
    case TermState(lb ~: rest, b, c, _)
      if (lb == 10 || lb == 13) => // Enter

      BasicFilters.injectNewLine(b, c, rest)
  }
  def readLine(prompt: String)(implicit ctx: Context): String = {
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
      //autocompleteFilter,
      cutPasteFilter,
      //multilineFilter,
      BasicFilters.all
    )

    Terminal.readLine(
      Console.BLUE + prompt + Console.RESET,
      reader,
      writer,
      allFilters,
      displayTransform = (buffer, cursor) => {
        val ansiBuffer = Ansi.Str.parse(SyntaxHighlighting(buffer))
        //val ansiBuffer = Ansi.Str.parse(SyntaxHighlighting(new SourceFile("<console>", buffer)))
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
      case Some(s) => history = s :: history; s
      case None => ":q"
    }
  }
}
