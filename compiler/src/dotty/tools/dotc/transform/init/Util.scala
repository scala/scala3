package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import Symbols._
import config.Printers.Printer


object Util {
  def traceIndented(msg: String, printer: Printer)(implicit ctx: Context): Unit =
    printer.println(s"${ctx.base.indentTab * ctx.base.indent} $msg")

  def traceOp(msg: String, printer: Printer)(op: => Unit)(implicit ctx: Context): Unit = {
    traceIndented(s"==> ${msg}", printer)
    op
    traceIndented(s"<== ${msg}", printer)
  }

  def (symbol: Symbol) isInternal(implicit ctx: Context): Boolean =
    !symbol.defTree.isEmpty

  def resolve(cls: ClassSymbol, sym: Symbol)(implicit ctx: Context): Symbol =
    if (sym.isEffectivelyFinal || sym.isConstructor) sym
    else sym.matchingMember(cls.typeRef)

  def resolveSuper(cls: ClassSymbol, superCls: ClassSymbol, sym: Symbol)(implicit ctx: Context): Symbol =
    sym.superSymbolIn(cls)
}