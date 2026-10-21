package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import org.junit.Test
import org.junit.Assert.*

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.toMessage
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

/** Tests that `Diagnostic.relatedInformation` / `diagnosticRelatedInformation` surface the inline
 *  call-site chain as structured related information (scalameta/metals#3214), so tooling can offer
 *  one clickable entry per inline site instead of just the final splice location.
 */
class DiagnosticRelatedInformationTest extends DottyTest:

  /** End-to-end: compile a real two-level inline chain whose innermost body fails to resolve a
   *  given, then assert the reported error carries one related-information entry per inline
   *  call-site, each pointing into the source. This exercises the actual `inlinePosStack` produced
   *  by inlining, not a hand-built position chain.
   */
  @Test def relatedInformationFromNestedInlineCompilation: Unit =
    val source =
      """|object Macros:
         |  trait TC[T]
         |  inline def inner[T]: TC[T] = scala.compiletime.summonInline[TC[T]]
         |  inline def outer[T]: TC[T] = inner[T]
         |  val test: TC[String] = outer[String]
         |""".stripMargin

    val reporter = new StoreReporter(null)
    ctx = initialCtx.setReporter(reporter)

    var diagnostics: List[Diagnostic] = Nil
    checkAfterCompile("inlining", List(source)) { rctx =>
      diagnostics = reporter.removeBufferedMessages(using rctx)
    }

    // The error from the failed `summonInline` is the one carrying inline related information.
    val withInlineInfo = diagnostics.filter(_.diagnosticRelatedInformation.size > 0)
    assertEquals(s"expected exactly one diagnostic with related info, got: $diagnostics", 1, withInlineInfo.size)

    val info = withInlineInfo.head.diagnosticRelatedInformation
    // One entry per surrounding inline call-site: `summonInline` in `inner` (line 3, 0-based 2) and
    // `inner[T]` in `outer` (line 4, 0-based 3).
    assertEquals(2, info.size)
    info.forEach(ri => assertEquals(Diagnostic.inlinedCodeMessage, ri.message))
    val lines = (0 until info.size).map(info.get(_).position.line).sorted
    assertEquals(List(2, 3), lines.toList)

  /** Unit-level: the structured list mirrors the inline call-site chain, excluding the outermost
   *  non-inlined position (which the primary diagnostic already points at). The chain is modeled
   *  directly with nested `SourcePosition`s (innermost splice site -> intermediate inline call-site
   *  -> outermost user call).
   */
  @Test def relatedInformationListsInlineCallSites: Unit =
    val userSrc = SourceFile.virtual("User.scala", "object Main { matchTwo }")
    val libSrc = SourceFile.virtual("Macros.scala", "inline def matchTwo = matchOne\ninline def matchOne = err")
    val posUser = SourcePosition(userSrc, Span(14, 22))
    val posMatchOne = SourcePosition(libSrc, Span(22, 30), posUser)
    val posError = SourcePosition(libSrc, Span(53, 56), posMatchOne)

    val dia = Diagnostic.Error("boom".toMessage, posError)
    val info = dia.diagnosticRelatedInformation
    assertEquals(2, info.size)
    assertEquals(posError, info.get(0).position)
    assertEquals(posMatchOne, info.get(1).position)
    assertEquals(Diagnostic.inlinedCodeMessage, info.get(0).message)
    // The interfaces view derives from the concrete `relatedInformation` source.
    assertEquals(List(posError, posMatchOne), dia.relatedInformation.map(_.pos))

  @Test def nonInlinedDiagnosticHasNoRelatedInformation: Unit =
    val src = SourceFile.virtual("User.scala", "object Main { matchTwo }")
    val dia = Diagnostic.Error("boom".toMessage, SourcePosition(src, Span(14, 22)))
    assertTrue(dia.diagnosticRelatedInformation.isEmpty)
end DiagnosticRelatedInformationTest
