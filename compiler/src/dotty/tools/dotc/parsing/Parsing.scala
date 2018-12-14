package dotty.tools.dotc.parsing

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.config.Config
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.util.{SourcePosition, NoSourcePosition}
import dotty.tools.dotc.util.Stats.record

class Parsing extends Phase {

  override def phaseName: String = "parsing"

  // We run TreeChecker only after type checking
  override def isCheckable: Boolean = false

  override def isRunnable(implicit ctx: Context): Boolean =
    !ctx.settings.fromTasty.value

  /** The position of the first XML literal encountered while parsing,
   *  NoSourcePosition if there were no XML literals.
   */
  private[this] var firstXmlPos: SourcePosition = NoSourcePosition

  override def run(implicit ctx: Context): Unit = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else {
        val p = new Parser(unit.source)
        val tree = p.parse()
        if (p.firstXmlPos.exists && !firstXmlPos.exists)
          firstXmlPos = p.firstXmlPos
        tree
      }

    record("parsedTrees", Trees.ntrees)

    if (firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists)
      ctx.error("""To support XML literals, your project must depend on scala-xml.
                  |See https://github.com/scala/scala-xml for more information.""".stripMargin,
        firstXmlPos)

    if (Config.checkPositions)
      unit.untpdTree.checkPos(nonOverlapping = !unit.isJava && !ctx.reporter.hasErrors)
  }
}
