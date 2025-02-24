package dotty.tools.debug

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.SymUtils

private class ExpressionStore:
  var symbol: TermSymbol | Null = null
  // To resolve captured variables, we store:
  // - All classes in the chain of owners of the expression
  // - The first local method enclosing the expression
  var classOwners: Seq[ClassSymbol] = Seq.empty
  var capturingMethod: Option[TermSymbol] = None

  def store(exprSym: Symbol)(using Context): Unit =
    symbol = exprSym.asTerm
    classOwners = exprSym.ownersIterator.collect { case cls: ClassSymbol => cls }.toSeq
    capturingMethod = exprSym.ownersIterator
      .find(sym => (sym.isClass || sym.is(Method)) && sym.enclosure.is(Method)) // the first local class or method
      .collect { case sym if sym.is(Method) => sym.asTerm } // if it is a method
