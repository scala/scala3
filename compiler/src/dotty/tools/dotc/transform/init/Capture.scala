package dotty.tools.dotc
package transform
package init

import core._
import MegaPhase._
import Contexts.Context
import StdNames._
import Names._
import Phases._
import ast._
import tpd._
import Flags._
import SymUtils._
import Symbols._
import Denotations._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import util.Positions._
import config.Printers.init.{ println => debug }
import Constants.Constant
import collection.mutable

object Capture {
  private class CaptureTraverser(inner: Symbol, outer: Symbol)(implicit ctx: Context) extends TreeTraverser {
    val captured: mutable.Set[Type] = mutable.Set.empty

    /** outer is inclusive, inner is exclusive */
    def within(sym: Symbol)(implicit ctx: Context): Boolean =
      sym.exists && sym != inner && (sym.owner == outer || within(sym.owner))

    def within(tp: Type)(implicit ctx: Context): Boolean = tp match {
      case tp : TermRef if tp.symbol.is(Module) && ctx.owner.enclosedIn(tp.symbol.moduleClass) =>
        // self reference by name: object O { ... O.xxx }
        within(ThisType.raw(tp.symbol.moduleClass.typeRef))
      case tp @ TermRef(NoPrefix, _) =>
        within(tp.symbol)
      case tp @ TermRef(prefix, _) =>
        within(prefix)
      case tp @ ThisType(tref) =>
        within(tref.symbol)
      case _ => false
    }

    def check(tp: Type) = if (within(tp)) captured += tp

    def traverse(tree: Tree)(implicit ctx: Context) = try { //debug
      def enclosure = ctx.owner.enclosingMethod

      tree match {
        case tree: Ident =>
          check(tree.tpe)
        case Trees.Select(ths: This, _) =>
          check(tree.tpe)
        case tree: Select =>
          traverseChildren(tree.qualifier)
        case tree: This =>
          captured += tree.tpe
        case _ =>
          traverseChildren(tree)
      }
    } catch { //debug
      case ex: Exception =>
        println(i"$ex while traversing $tree")
        throw ex
    }
  }

  def analyze(inner: Symbol, outer: Symbol)(implicit setting: Setting): Set[Type] =
    new CaptureTraverser(inner, outer).captured.toSet
}
