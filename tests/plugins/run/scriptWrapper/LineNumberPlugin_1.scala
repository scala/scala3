package scriptWrapper

import dotty.tools.dotc.*
import core.*
import Contexts.Context
import Contexts.ctx
import plugins.*
import ast.tpd
import util.SourceFile

class LineNumberPlugin extends StandardPlugin {
  val name: String = "linenumbers"
  val description: String = "adjusts line numbers of script files"

  override def initialize(options: List[String])(using Context): List[PluginPhase] = FixLineNumbers() :: Nil
}

// Loosely follows Mill linenumbers plugin (scan for marker with "original" source, adjust line numbers to match)
class FixLineNumbers extends PluginPhase {

  val codeMarker = "//USER_CODE_HERE"

  def phaseName: String = "fixLineNumbers"
  override def runsAfter: Set[String] = Set("posttyper")
  override def runsBefore: Set[String] = Set("pickler")

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    val sourceContent = ctx.source.content()
    val lines = new String(sourceContent).linesWithSeparators.toVector
    val codeMarkerLine = lines.indexWhere(_.startsWith(codeMarker))

    if codeMarkerLine < 0 then
      tree
    else
      val adjustedFile = lines.collectFirst {
        case s"//USER_SRC_FILE:./$file" => file.trim
      }.getOrElse("<unknown>")

      val adjustedSrc = ctx.source.file.container.lookupName(adjustedFile, directory = false) match
        case null =>
          report.error(s"could not find file $adjustedFile", tree.sourcePos)
          return tree
        case file =>
          SourceFile(file, scala.io.Codec.UTF8)

      val userCodeOffset = ctx.source.lineToOffset(codeMarkerLine + 1) // lines.take(codeMarkerLine).map(_.length).sum
      val lineMapper = LineMapper(codeMarkerLine, userCodeOffset, adjustedSrc)
      lineMapper.transform(tree)
  }

}

class LineMapper(markerLine: Int, userCodeOffset: Int, adjustedSrc: SourceFile) extends tpd.TreeMapWithPreciseStatContexts() {

  override def transform(tree: tpd.Tree)(using Context): tpd.Tree = {
    val tree0 = super.transform(tree)
    val pos = tree0.sourcePos
    if pos.exists && pos.start >= userCodeOffset then
      val tree1 = tree0.cloneIn(adjustedSrc).withSpan(pos.span.shift(-userCodeOffset))
      // if tree1.show.toString == "???" then
      //   val pos1 = tree1.sourcePos
      //   sys.error(s"rewrote ??? at ${pos1.source}:${pos1.line + 1}:${pos1.column + 1} (sourced from ${markerLine + 2})")
      tree1
    else
      tree0
  }

}
