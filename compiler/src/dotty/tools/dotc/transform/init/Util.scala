package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Symbols._
import config.Printers.Printer

import annotation.tailrec

object Util {
  def traceIndented(msg: String, printer: Printer)(using Context): Unit =
    printer.println(s"${ctx.base.indentTab * ctx.base.indent} $msg")

  def traceOp(msg: String, printer: Printer)(op: => Unit)(using Context): Unit = {
    traceIndented(s"==> ${msg}", printer)
    op
    traceIndented(s"<== ${msg}", printer)
  }

  extension (symbol: Symbol) def hasSource(using Context): Boolean =
    !symbol.defTree.isEmpty

  def resolve(cls: ClassSymbol, sym: Symbol)(using Context): Symbol =
    if (sym.isEffectivelyFinal || sym.isConstructor) sym
    else sym.matchingMember(cls.appliedRef)

  def resolveSuper(cls: ClassSymbol, superCls: ClassSymbol, sym: Symbol)(using Context): Symbol = {
    // println(s"bases of $cls: " + cls.info.baseClasses)
    @tailrec def loop(bcs: List[ClassSymbol]): Symbol = bcs match {
      case bc :: bcs1 =>
        val cand = sym.matchingDecl(bcs.head, cls.thisType)
          .suchThat(alt => !alt.is(Flags.Deferred)).symbol
        if (cand.exists) cand else loop(bcs.tail)
      case _ =>
        NoSymbol
    }
    loop(cls.info.baseClasses.dropWhile(sym.owner != _))
  }
}