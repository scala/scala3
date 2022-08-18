package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.PositionContext
import dotty.tools.languageserver.util.embedded.CodeMarker

import dotty.tools.dotc.util.Signatures.Signature

import org.eclipse.lsp4j.{MarkupContent, ParameterInformation, SignatureInformation}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

import scala.jdk.CollectionConverters._

import SignatureHelp._

/**
 * An action requesting for signature help at `marker`.
 * This action corresponds to the `textDocument/signatureHelp` method of the Language
 * Server Protocol.
 *
 * @param marker          The marker indicating the position where the signature help should be
 *                        requested.
 * @param expected        The results that are expected for this query.
 * @param activeSignature The expected index of the active signature.
 * @param activeParam     The expected index of the active paremeter.
 */
class SignatureHelp(override val marker: CodeMarker,
                    expected: List[Signature],
                    activeSignature: Option[Int],
                    activeParam: Int) extends ActionOnMarker {

  val expectedSignatures = expected.map(DottyLanguageServer.signatureToSignatureInformation)

  override def execute(): Exec[Unit] = {
    val results = server.signatureHelp(marker.toTextDocumentPositionParams).get()
    val resultSignatures = results.getSignatures.asScala

    assertEquals("Number of signatures", expected.length, resultSignatures.length)

    // We don't check that we get the same active signature, just that the signature at
    // `activeSignature` is the same, if any signature can be considered "active".
    activeSignature match {
      case Some(active) if expectedSignatures.nonEmpty =>
        val expectedActive = expectedSignatures(active)
        val resultActive = resultSignatures(results.getActiveSignature)
        assertEquals("active signature", expectedActive, resultActive)
      case _ =>
        ()
    }

    assertEquals("activeParam", activeParam, results.getActiveParameter)

    expectedSignatures.sorted.zip(resultSignatures.sorted).foreach {
      assertEquals(_, _)
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"SignatureHelp(${marker.show}, $expected, $activeSignature, $activeParam)"
}

object SignatureHelp {

  implicit val signatureInformationOrdering: Ordering[SignatureInformation] = Ordering.by(_.toString)
}
