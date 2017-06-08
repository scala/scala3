package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer
import dotc.core.Contexts.Context
import dotc.parsing.Parsers
import dotc.util.SourceFile
import dotc.ast.untpd
import dotc.reporting._

private[repl] enum ParseResult {
  case Trees(xs: Seq[untpd.Tree])
  case SyntaxErrors(xs: Seq[MessageContainer])
  case Incomplete
}

private[repl] object replParse {
  import ParseResult._

  def apply(sourceCode: String)(implicit ctx: Context): ParseResult = {
    val reporter =
      new StoreReporter(null)
      with UniqueMessagePositions
      with HideNonSensicalMessages

    var needsMore = false
    reporter.withIncompleteHandler(_ => _ => needsMore = true) {
      implicit val myCtx = ctx.fresh.setReporter(reporter)

      val source = new SourceFile("<console>", sourceCode.toCharArray)
      val parser = new Parsers.Parser(source)(myCtx)

      val (_, stats) = parser.templateStatSeq

      if (reporter.hasErrors) SyntaxErrors(reporter.removeBufferedMessages)
      else if (needsMore) Incomplete
      else Trees(stats)
    }
  }
}
