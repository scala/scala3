package dotty.tools.languageserver.util.embedded

import dotty.tools.languageserver.util.CodeRange

case class CodeInRange(text: String, range: CodeRange) extends Embedded
