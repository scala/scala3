package dotty.tools.repl.worksheet

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition

private final case class InputStatement(start: Int, end: Int, original: SourceFile):
  val source: String = original.content.slice(start, end).mkString
  val position: WorksheetPosition = WorksheetPosition.fromOffsets(original, start, end)

  def mapPosition(generated: SourcePosition): WorksheetPosition =
    if !generated.exists then position
    else
      val from = math.min(start + generated.start, end)
      val to = math.min(start + generated.end, end)
      WorksheetPosition.fromOffsets(original, from, to)

private object WorksheetSource:
  def statements(original: SourceFile, trees: List[untpd.Tree]): List[InputStatement] =
    trees.collect:
      case tree if tree.span.exists =>
        InputStatement(tree.span.start, tree.span.end, original)
