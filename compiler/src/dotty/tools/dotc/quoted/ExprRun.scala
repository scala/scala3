package dotty.tools.dotc
package quoted

import dotty.tools.dotc.core.Contexts._

import scala.quoted._

class ExprRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  def compileExpr(expr: Expr[_]): Unit = {
    val units = new ExprCompilationUnit(expr) :: Nil
    compileUnits(units)
  }
}
