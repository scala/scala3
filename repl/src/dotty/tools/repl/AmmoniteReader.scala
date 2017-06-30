package dotty.tools
package repl

import java.io.{ OutputStreamWriter, InputStreamReader }

import dotc.core.Contexts.Context
import dotc.printing.SyntaxHighlighting
import dotc.printing.Highlighting
import dotc.interactive.InteractiveDriver

import terminal._
import terminal.filters._
import GUILikeFilters._
import LazyList._
import AmmoniteReader._

private[repl] class AmmoniteReader(history: History,
                                   complete: (Int, String) => (Int, Seq[String], Seq[String]))
                                  (implicit ctx: Context) {

  private[this] val reader = new InputStreamReader(System.in)
  private[this] val writer = new OutputStreamWriter(System.out)

  private[this] val cutPasteFilter  = ReadlineFilters.CutPasteFilter()
  private[this] val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)
  private[this] val multilineFilter = Filter.partial("multilineFilter") {
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

    val autocompleteFilter: Filter = Filter.action("autocompleteFilter")(SpecialKeys.Tab :: Nil) {
      case TermState(rest, b, c, _) =>
        val (newCursor, completions, details) = complete(c, b.mkString)
        lazy val prefixDetails = FrontEndUtils.findPrefix(details)
        val details2 = details.map { det =>
          val (left, right) = det.splitAt(prefixDetails.length)
          (Highlighting.Green(left) + Highlighting.Cyan(right)).toString
        }

        lazy val prefixComp = FrontEndUtils.findPrefix(completions)
        val completions2: Seq[String] = for(comp <- completions) yield {
          val (left, right) = comp.splitAt(prefixComp.length)
          (Highlighting.Green(left).toString ++ right)
        }

        val stdout = FrontEndUtils.printCompletions(completions2, details2).mkString

        if (details.nonEmpty || completions.isEmpty)
          Printing(TermState(rest, b, c), stdout)
        else {
          val newBuffer = b.take(newCursor) ++ prefixComp ++ b.drop(c)
          Printing(TermState(rest, newBuffer, newCursor + prefixComp.length), stdout)
        }
    }

    val allFilters = Filter.merge(
      UndoFilter(),
      historyFilter,
      selectionFilter,
      GUILikeFilters.altFilter,
      GUILikeFilters.fnFilter,
      ReadlineFilters.navFilter,
      cutPasteFilter,
      autocompleteFilter,
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
      .map {
        case Result(source) =>
          (ParseResult(source), source :: history)
        case Interrupt =>
          (SigKill, history)
      }
      .getOrElse((Quit, history))
  }
}

object AmmoniteReader {
  type History = List[String]

  def apply(history: History,
            complete: (Int, String) => (Int, Seq[String], Seq[String]))
           (implicit ctx: Context) =
    new AmmoniteReader(history,complete)
}
