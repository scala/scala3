package dotty.tools.pc.utils

import scala.collection.immutable
import scala.language.unsafeNulls

import dotty.tools.pc.utils.TestExtensions.*

import org.eclipse.lsp4j.{DocumentHighlight, Range}

trait RangeReplace:

  def renderHighlightsAsString(
      code: String,
      highlights: List[DocumentHighlight]
  ): String = renderRangesAsString(code, highlights.map(_.getRange()))

  def renderRangesAsString(
      code: String,
      highlights: List[Range],
      alreadyAddedMarkings: List[(Int, Int)] = Nil,
      currentBase: Option[String] = None
  ): String =
    highlights
      .foldLeft((currentBase.getOrElse(code), alreadyAddedMarkings)) {
        case ((base, alreadyAddedMarkings), range) =>
          replaceInRangeWithAdjustments(
            code,
            base,
            range,
            alreadyAddedMarkings
          )
      }
      ._1

  protected def replaceInRange(
      base: String,
      range: Range,
      prefix: String = "<<",
      suffix: String = ">>"
  ): String =
    replaceInRangeWithAdjustments(base, base, range, List(), prefix, suffix)._1

  protected def replaceInRangeWithAdjustments(
      code: String,
      currentBase: String,
      range: Range,
      alreadyAddedMarkings: List[(Int, Int)],
      prefix: String = "<<",
      suffix: String = ">>"
  ): (String, List[(Int, Int)]) =
    def adjustPosition(pos: Int) =
      alreadyAddedMarkings
        .filter { case (i, _) => i <= pos }
        .map(_._2)
        .fold(0)(_ + _) + pos

    val posStart = adjustPosition(range.getStart.getOffset(code))
    val posEnd = adjustPosition(range.getEnd.getOffset(code))
    (
      new java.lang.StringBuilder()
        .append(currentBase, 0, posStart)
        .append(prefix)
        .append(currentBase, posStart, posEnd)
        .append(suffix)
        .append(currentBase, posEnd, currentBase.length)
        .toString,
      (posStart, prefix.length) :: (
        posEnd,
        suffix.length
      ) :: alreadyAddedMarkings
    )
