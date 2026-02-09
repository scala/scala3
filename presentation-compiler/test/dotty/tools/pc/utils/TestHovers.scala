package dotty.tools.pc.utils

import scala.language.unsafeNulls
import scala.meta.internal.pc.HoverMarkup

import dotty.tools.pc.utils.TestExtensions.*

import org.eclipse.lsp4j.Hover

trait TestHovers:
  extension (string: String)
    def hover: String =
      string.trim.linesIterator.toList match
        case List(symbolSignature) =>
          HoverMarkup("", Some(symbolSignature), "")
        case List(expressionType, symbolSignature) =>
          HoverMarkup(
            expressionType,
            Some(symbolSignature),
            "",
            forceExpressionType = true
          )
        case _ =>
          string

    def hoverRange: String =
      string.trim.linesIterator.toList match
        case List(symbolSignature) =>
          HoverMarkup(symbolSignature)
        case List(expressionType, symbolSignature) =>
          HoverMarkup(
            expressionType,
            Some(symbolSignature),
            "",
            forceExpressionType = true
          )
        case _ =>
          string

  def renderAsString(
      code: String,
      hover: Option[Hover],
      includeRange: Boolean
  ): String =
    hover match
      case Some(value) =>
        val types = value.getContents.getRight.getValue()

        val range = Option(value.getRange) match
          case Some(value) if includeRange =>
            val start = value.getStart.getOffset(code)
            val end = value.getEnd.getOffset(code)
            val rangeText = code.slice(start, end)
            codeFence(
              rangeText,
              "range"
            )
          case _ => ""
        List(types, range).filterNot(_.isEmpty).mkString("\n")
      case None =>
        ""

  private def codeFence(code: String, language: String): String =
    val trimmed = code.trim
    if trimmed.isEmpty then ""
    else
      new StringBuilder()
        .append("```")
        .append(language)
        .append("\n")
        .append(trimmed)
        .append("\n")
        .append("```")
        .toString()
