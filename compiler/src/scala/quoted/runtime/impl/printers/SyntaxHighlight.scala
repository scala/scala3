package scala.quoted
package runtime.impl.printers

import dotty.shaded.fansi

trait SyntaxHighlight {
  def highlightKeyword(str: String): String
  def highlightTypeDef(str: String): String
  def highlightLiteral(str: String): String
  def highlightValDef(str: String): String
  def highlightOperator(str: String): String
  def highlightAnnotation(str: String): String
  def highlightString(str: String): String
  def highlightTripleQs: String
}

object SyntaxHighlight {

  def ANSI: SyntaxHighlight = new SyntaxHighlight {
    // Keep in sync with SyntaxHighlighting
    def highlightKeyword(str: String): String = fansi.Color.Yellow(str).render
    def highlightTypeDef(str: String): String = fansi.Color.Magenta(str).render
    def highlightLiteral(str: String): String = fansi.Color.Red(str).render
    def highlightValDef(str: String): String = fansi.Color.Cyan(str).render
    def highlightOperator(str: String): String = fansi.Color.Magenta(str).render
    def highlightAnnotation(str: String): String = fansi.Color.Magenta(str).render
    def highlightString(str: String): String = fansi.Color.Green(str).render
    def highlightTripleQs: String = fansi.Back.Red("???").render
  }

  def plain: SyntaxHighlight = new SyntaxHighlight {
    def highlightKeyword(str: String): String = str
    def highlightTypeDef(str: String): String = str
    def highlightLiteral(str: String): String = str
    def highlightValDef(str: String): String = str
    def highlightOperator(str: String): String = str
    def highlightAnnotation(str: String): String = str
    def highlightString(str: String): String = str
    def highlightTripleQs: String = "???"
  }
}
