/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tasty
package util

object SyntaxHighlightUtils {

  // Keep in sync with SyntaxHighlighting
  private val NoColor         = Console.RESET
  private val CommentColor    = Console.BLUE
  private val KeywordColor    = Console.YELLOW
  private val ValDefColor     = Console.CYAN
  private val LiteralColor    = Console.RED
  private val StringColor     = Console.GREEN
  private val TypeColor       = Console.MAGENTA
  private val AnnotationColor = Console.MAGENTA

  def highlightKeyword(str: String, withColor: Boolean) = if (withColor) { KeywordColor + str + NoColor } else str
  def highlightTypeDef(str: String, withColor: Boolean) = if (withColor) { TypeColor + str + NoColor } else str
  def highlightLiteral(str: String, withColor: Boolean) = if (withColor) { LiteralColor + str + NoColor } else str
  def highlightValDef(str: String, withColor: Boolean) = if (withColor) { ValDefColor + str + NoColor } else str
  def highlightOperator(str: String, withColor: Boolean) = if (withColor) { TypeColor + str + NoColor } else str
  def highlightAnnotation(str: String, withColor: Boolean) = if (withColor) { AnnotationColor + str + NoColor } else str
  def highlightString(str: String, withColor: Boolean) = if (withColor) { StringColor + str + NoColor } else str
  val tripleQs = Console.RED_B + "???" + NoColor

}