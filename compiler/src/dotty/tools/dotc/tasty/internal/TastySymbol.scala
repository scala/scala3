package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._

import scala.tasty.statements

object TastySymbol {

  def apply(sym: Symbol)(implicit ctx: Context): scala.tasty.Symbol =
    if (sym.exists) new Impl(sym, ctx) else scala.tasty.NoSymbol

  private class Impl(sym: Symbol, ctx: Context) extends scala.tasty.Symbol { self =>

    override def name: String = sym.name(ctx).toString

    override def owner: scala.tasty.Symbol = TastySymbol(sym.denot(ctx).owner)(ctx)

    override def definition: Option[statements.Definition] = None // TODO return definition if it is known

    def ownersIterator: Iterator[scala.tasty.Symbol] = new Iterator[scala.tasty.Symbol] {
      private[this] var current: scala.tasty.Symbol = self.owner
      override def hasNext = current != scala.tasty.NoSymbol
      override def next() = {
        val c = current
        current = current.owner
        c
      }
    }

    override def toString: String =
      s"Symbol(${ownersIterator.foldLeft(name)((acc, s) => s"${s.name}.$acc")})"
  }
}
