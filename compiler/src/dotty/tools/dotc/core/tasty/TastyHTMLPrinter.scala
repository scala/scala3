package dotty.tools.dotc
package core
package tasty

import dotty.tools.tasty.TastyBuffer.NameRef

import Contexts._, Decorators._
import Names.Name
import TastyUnpickler._
import util.Spans.offsetToInt
import printing.Highlighting._

class TastyHTMLPrinter(bytes: Array[Byte])(implicit ctx: Context) extends TastyPrinter(bytes) {
  override protected def nameColor(str: String): String = s"<span class='name'>$str</span>"
  override protected def treeColor(str: String): String = s"<span class='tree'>$str</span>"
  override protected def lengthColor(str: String): String = s"<span class='length'>$str</span>"
}
