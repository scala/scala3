package scala.quoted
package runtime.impl.printers

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
    private val NoColor         = Console.RESET
    private val CommentColor    = Console.BLUE
    private val KeywordColor    = Console.YELLOW
    private val ValDefColor     = Console.CYAN
    private val LiteralColor    = Console.RED
    private val StringColor     = Console.GREEN
    private val TypeColor       = Console.MAGENTA
    private val AnnotationColor = Console.MAGENTA

    def highlightKeyword(str: String): String = KeywordColor + str + NoColor
    def highlightTypeDef(str: String): String = TypeColor + str + NoColor
    def highlightLiteral(str: String): String = LiteralColor + str + NoColor
    def highlightValDef(str: String): String = ValDefColor + str + NoColor
    def highlightOperator(str: String): String = TypeColor + str + NoColor
    def highlightAnnotation(str: String): String = AnnotationColor + str + NoColor
    def highlightString(str: String): String = StringColor + str + NoColor
    def highlightTripleQs: String = Console.RED_B + "???" + NoColor
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
