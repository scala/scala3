package dotty.tools
package dotc
package typer

import ast.untpd._
import core._
import Symbols._, Names._, Denotations._, Types._, Contexts._

case class ImportInfo(sym: Symbol, selectors: List[Tree], scopeNestingLevel: Int) {
  /** The (TermRef) type of the qualifier of the import clause */
  def site(implicit ctx: Context): Type = {
    val ImportType(expr) = sym.info
    expr.tpe
  }
}