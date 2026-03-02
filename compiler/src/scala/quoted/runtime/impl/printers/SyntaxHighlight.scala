package scala.quoted
package runtime.impl.printers

trait SyntaxHighlight {
  def highlightKeyword(str: String): String
  def highlightType(str: String): String
  def highlightLiteral(str: String): String
  def highlightDefinition(str: String): String
  def highlightTripleQs: String
}

object SyntaxHighlight {

  def ANSI: SyntaxHighlight = new SyntaxHighlight {
    // Keep in sync with SyntaxHighlighting
    private val NoColor         = Console.RESET
    private val CommentColor    = Console.BLUE
    private val KeywordColor    = Console.YELLOW
    private val DefinitionColor = Console.CYAN
    private val LiteralColor    = Console.GREEN
    private val TypeColor       = Console.GREEN

    def highlightKeyword(str: String): String = KeywordColor + str + NoColor
    def highlightType(str: String): String = TypeColor + str + NoColor
    def highlightLiteral(str: String): String = LiteralColor + str + NoColor
    def highlightDefinition(str: String): String = DefinitionColor + str + NoColor
    def highlightTripleQs: String = Console.RED_B + "???" + NoColor
  }

  def plain: SyntaxHighlight = new SyntaxHighlight {
    def highlightKeyword(str: String): String = str
    def highlightType(str: String): String = str
    def highlightLiteral(str: String): String = str
    def highlightDefinition(str: String): String = str
    def highlightTripleQs: String = "???"
  }
}
