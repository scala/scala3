package dotty.tools.dotc

import util.Chars.*
import core.Names.Name
import core.StdNames.nme
import core.NameOps.*

package object parsing {

  /**
   * Compute the precedence of infix operator `operator` according to the SLS [§ 6.12.3][SLS].
   * We implement [SIP-33][SIP-33] and give type operators the same precedence as term operators.
   *
   * [SLS]: https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#infix-operations
   * [SIP-33]: https://docs.scala-lang.org/sips/priority-based-infix-type-precedence.html
   */
  def precedence(operator: Name): Int =
    if (operator eq nme.ERROR) -1
    else {
      val firstCh = operator.firstCodePoint
      if (isScalaLetter(firstCh)) 1
      else if (operator.isOpAssignmentName) 0
      else firstCh match {
        case '|' => 2
        case '^' => 3
        case '&' => 4
        case '=' | '!' => 5
        case '<' | '>' => 6
        case ':' => 7
        case '+' | '-' => 8
        case '*' | '/' | '%' => 9
        case _ => 10
      }
    }

  def minPrec: Int = 0
  def minInfixPrec: Int = 1
  def maxPrec: Int = 11
}

