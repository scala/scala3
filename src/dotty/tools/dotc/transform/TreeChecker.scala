package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.Decorators._
import core.transform.Erasure.isUnboundedGeneric
import typer._
import typer.ErrorReporting._
import reporting.ThrowingReporter
import ast.Trees._
import ast.{tpd, untpd}
import java.lang.AssertionError

/** This transform eliminates patterns. Right now it's a dummy.
 *  Awaiting the real pattern matcher.
 */
class TreeChecker {
  import ast.tpd._

  def check(ctx: Context) = {
    println(s"checking ${ctx.compilationUnit} after phase ${ctx.phase.prev}")
    val checkingCtx = ctx.fresh
      .setTyperState(ctx.typerState.withReporter(new ThrowingReporter(ctx.typerState.reporter)))
    Checker.typedExpr(ctx.compilationUnit.tpdTree)(checkingCtx)
  }

  object Checker extends ReTyper {
    override def typed(tree: untpd.Tree, pt: Type)(implicit ctx: Context) = try {
      tree match {
        case _: untpd.UnApply =>
          // can't recheck patterns
          tree.asInstanceOf[tpd.Tree]
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[_] =>
          super.typed(tree)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typed(tree, pt)
          def sameType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 =:= tp2)
          def divergenceMsg =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
             """.stripMargin
          assert(sameType(tree1.tpe, tree.typeOpt), divergenceMsg)
          tree1
        }
    } catch {
      case ex: Throwable =>
        println(i"exception while checking $tree of class ${tree.getClass} # ${tree.uniqueId}")
        throw ex
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || ctx.phase.prev.id <= ctx.typerPhase.id, tree.show + " at " + ctx.phase)
      super.typedIdent(tree, pt)
    }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || ctx.phase.prev.id <= ctx.typerPhase.id, tree.show + " at " + ctx.phase)
      super.typedSelect(tree, pt)
    }
  }
}

object TreeChecker extends TreeChecker