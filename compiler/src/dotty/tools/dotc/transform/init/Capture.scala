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
  private class CaptureTraverser(setting: Setting) extends TreeTraverser {
    val captured: mutable.Map[Type, List[Tree]] = mutable.Map.empty

    def trace[T](msg: => String)(body: => T) = {
      println(msg)
      val res = body
      println(msg + s" = $res" )
      res
    }

    def free(tp: Type)(implicit ctx: Context): Boolean = (tp match {
      case tp : TermRef if tp.symbol.is(Module) && ctx.owner.enclosedIn(tp.symbol.moduleClass) =>
        // self reference by name: object O { ... O.xxx }
        free(ThisType.raw(tp.symbol.moduleClass.typeRef))
      case tp @ TermRef(NoPrefix, _) =>
        setting.env.contains(tp.symbol)
      case tp: NamedType =>
        free(tp.prefix)
      case tp @ ThisType(tref) =>
        val cls = tref.symbol
        !cls.is(Package) && setting.env.contains(cls)
      case _ => false
    })

    def check(tree: Tree)(implicit ctx: Context) =
      if (free(tree.tpe)) captured(tree.tpe) = tree :: captured.getOrElseUpdate(tree.tpe, Nil)

    def traverse(tree: Tree)(implicit ctx: Context) = try { //debug
      tree match {
        case tree if tree.isType =>
          // ignore all type trees
        case Trees.Typed(expr, tpt) if tpt.tpe.hasAnnotation(defn.UncheckedAnnot) =>
          // ignore @unchecked
        case tree: Ident if tree.tpe.isInstanceOf[TermRef] =>
          // The condition is required, see capture1.scala
          check(tree)
        case Trees.Select(ths: This, _) =>
          check(tree)
        case tree: Select =>
          traverse(tree.qualifier)
        case tree: This =>
          check(tree)
        case tree: New =>
          check(tree)
        case _ =>
          traverseChildren(tree)
      }
    } catch { //debug
      case ex: Exception =>
        println(i"$ex while traversing $tree")
        throw ex
    }
  }

  def analyze(tree: Tree)(implicit setting: Setting): Map[Type, List[Tree]] = {
    val cap = new CaptureTraverser(setting)
    cap.traverse(tree)
    cap.captured.toMap
  }
}
