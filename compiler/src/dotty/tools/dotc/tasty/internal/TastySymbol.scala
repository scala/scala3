package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._

import scala.tasty.trees
import scala.tasty.names

object TastySymbol {

  def apply(sym: Symbol)(implicit ctx: Context): scala.tasty.Symbol =
    if (sym.exists) Impl(sym)(ctx) else scala.tasty.NoSymbol

  private case class Impl(sym: Symbol)(ctx: Context) extends scala.tasty.Symbol { self =>

    override def name: names.Name = Name(sym.name(ctx))

    override def owner: scala.tasty.Symbol = TastySymbol(sym.denot(ctx).owner)(ctx)

    override def definition: Option[trees.Definition] = None // TODO return definition if it is known

    override def toString: String =
      s"Symbol(${sym.showFullName(ctx)})"
  }
}
