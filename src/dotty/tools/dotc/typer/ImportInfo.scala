package dotty.tools
package dotc
package typer

import ast.untpd._
import core._
import Symbols._

case class ImportInfo(sym: Symbol, selectors: List[Tree], scopeNestingLevel: Int) {

}