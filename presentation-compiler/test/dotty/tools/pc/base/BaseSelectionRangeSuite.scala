package dotty.tools.pc.base

import java.nio.file.Paths
import java.util as ju

import scala.collection.immutable
import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}
import scala.meta.pc.OffsetParams

import dotty.tools.pc.utils.TestExtensions.*

import org.eclipse.lsp4j as l

abstract class BaseSelectionRangeSuite extends BasePCSuite:

  def check(
      original: String,
      expectedRanges: List[String],
      compat: Map[String, List[String]] = immutable.Map.empty
  ): Unit =
    val (code, offset) = params(original)
    val offsetParams: ju.List[OffsetParams] = List[OffsetParams](
      CompilerOffsetParams(
        Paths.get("SelectionRange.scala").toUri(),
        code,
        offset,
        EmptyCancelToken
      )
    ).asJava

    val selectionRanges = presentationCompiler
      .selectionRange(offsetParams)
      .get()
      .asScala
      .toList

    // Note that this takes some liberty since the spec isn't the clearest on
    // the way this is done and different LSP clients seems to impliment this
    // differently. Mainly, we mimic what VS Code does and what other servers
    // like jdtls do and we send in a single position. Once that positions is
    // recieved we check the Selection range and all the parents in that range
    // to ensure it contains the positions we'd expect it to contain. In the
    // case of VS Code, they never make a second request, and instead they
    // just rely on the tree to continue selecting. So we mimic that here in
    // the tests.
    assertSelectionRanges(
      selectionRanges.headOption,
      expectedRanges,
      code
    )

  private def assertSelectionRanges(
      range: Option[l.SelectionRange],
      expected: List[String],
      original: String
  ): Unit =
    assert(range.nonEmpty)
    expected.headOption.foreach { expectedRange =>
      val obtained = applyRanges(original, range.get)
      assertNoDiff(expectedRange, obtained)
      assertSelectionRanges(range.map(_.getParent()), expected.tail, original)
    }

  private def applyRanges(
      text: String,
      selectionRange: l.SelectionRange
  ): String =

    val (startPos, endPos) =
      (selectionRange.getRange.getStart, selectionRange.getRange.getEnd)
    val startOffset = startPos.getOffset(text)
    val endOffset = endPos.getOffset(text)

    val out = new java.lang.StringBuilder()
    out.append(text, 0, startOffset)
    out.append(">>region>>")
    out.append(text, startOffset, endOffset)
    out.append("<<region<<")
    out.append(text, endOffset, text.length)
    out.toString
