package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Flags._

import scala.tasty
import scala.tasty.trees

trait Definition extends trees.Definition {

  protected def tree: tpd.Tree

  def owner(implicit tctx: scala.tasty.Context): trees.Definition = {
    implicit val ctx = tctx.asInstanceOf[TastyContext].ctx
    Definition(tree.symbol.owner)
  }

  def localContext(implicit tctx: tasty.Context): tasty.Context = {
    implicit val ctx = tctx.asInstanceOf[TastyContext].ctx
    new TastyContext(
      if (tree.hasType && tree.symbol.exists) ctx.withOwner(tree.symbol)
      else ctx
    )
  }
}

object Definition {

  def apply(tree: tpd.Tree)(implicit ctx: Context): trees.Definition = tree match {
    case tree: tpd.ValDef => ValDef(tree)
    case tree: tpd.DefDef => DefDef(tree)
    case tree: tpd.TypeDef =>
      if (tree.symbol.isClass) ClassDef(tree)
      else TypeDef(tree)
  }

  def apply(sym: Symbol)(implicit ctx: Context): trees.Definition = {
    if (sym.is(Package)) PackageDef(sym)
    else if (sym == defn.AnyClass) NoDefinition // FIXME
    else if (sym == defn.NothingClass) NoDefinition // FIXME
    else if (sym.isClass) ClassDef(sym.asClass)
    else if (sym.isType) TypeDef(sym.asType)
    else if (sym.is(Method)) DefDef(sym.asTerm)
    else ValDef(sym.asTerm)
  }

  private[tasty] object NoDefinition extends trees.Definition {
    def owner(implicit tctx: scala.tasty.Context): trees.Definition = NoDefinition
    def localContext(implicit ctx: tasty.Context): tasty.Context = ctx
    def pos(implicit ctx: scala.tasty.Context): scala.tasty.Position = ???
    override def toString: String = "NoDefinition"
  }

}
