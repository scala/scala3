package dotty.tools
package dotc
package reporting

import org.junit.Test
import org.junit.Assert.*

import dotty.tools.dotc.core.Decorators.toMessage
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

/** Tests that `Diagnostic.relatedInformation` / `diagnosticRelatedInformation` surface the inline
 *  call-site chain as structured related information (scalameta/metals#3214). The chain is modeled
 *  directly with nested `SourcePosition`s (innermost splice site → intermediate inline call-site →
 *  outermost user call), which mirrors what the compiler produces when an error occurs in code
 *  inlined through several `inline def`s.
 */
class DiagnosticRelatedInformationTest:

  private val userSrc = SourceFile.virtual("User.scala", "object Main { matchTwo }")
  private val libSrc = SourceFile.virtual("Macros.scala", "inline def matchTwo = matchOne\ninline def matchOne = err")

  // Outermost: the user's call site (non-inlined). Has no outer.
  private val posUser = SourcePosition(userSrc, Span(14, 22))
  // Intermediate inline call-site (matchOne, called inside matchTwo).
  private val posMatchOne = SourcePosition(libSrc, Span(22, 30), posUser)
  // Innermost splice site where the error is physically reported.
  private val posError = SourcePosition(libSrc, Span(53, 56), posMatchOne)

  @Test def relatedInformationListsInlineCallSites: Unit =
    val dia = Diagnostic.Error("boom".toMessage, posError)
    val info = dia.diagnosticRelatedInformation
    // One entry per inline call-site surrounding the error, excluding the outermost non-inlined
    // position (which the primary diagnostic already points at).
    assertEquals(2, info.size)
    assertEquals(posError, info.get(0).position)
    assertEquals(posMatchOne, info.get(1).position)
    assertEquals(Diagnostic.inlinedCodeMessage, info.get(0).message)
    // The interfaces view derives from the concrete `relatedInformation` source.
    assertEquals(List(posError, posMatchOne), dia.relatedInformation.map(_.pos))

  @Test def nonInlinedDiagnosticHasNoRelatedInformation: Unit =
    val dia = Diagnostic.Error("boom".toMessage, posUser)
    assertTrue(dia.diagnosticRelatedInformation.isEmpty)
end DiagnosticRelatedInformationTest
