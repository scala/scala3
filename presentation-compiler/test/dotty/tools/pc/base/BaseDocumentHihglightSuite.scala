package dotty.tools.pc.base

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}

import dotty.tools.pc.utils.RangeReplace

import org.eclipse.lsp4j.DocumentHighlight

class BaseDocumentHighlightSuite extends BasePCSuite with RangeReplace:

  def check(
      original: String
  ): Unit =
    val edit = original.replaceAll("(<<|>>)", "")
    val expected = original.replaceAll("@@", "")
    val base = original.replaceAll("(<<|>>|@@)", "")

    val (code, offset) = params(edit)
    val highlights = presentationCompiler
      .documentHighlight(
        CompilerOffsetParams(
          URI.create("file:/Highlight.scala"),
          code,
          offset,
          EmptyCancelToken
        )
      )
      .get()
      .asScala
      .toList
      .sortWith(compareHighlights)
      .reverse

    val obtained = renderHighlightsAsString(base, highlights)
    assertNoDiff(expected, obtained)

  private def compareHighlights(h1: DocumentHighlight, h2: DocumentHighlight) =
    val r1 = h1.getRange().getStart()
    val r2 = h2.getRange().getStart()
    r1.getLine() < r2.getLine() || (r1.getLine() == r2.getLine() && r1
      .getCharacter() < r2.getCharacter())
