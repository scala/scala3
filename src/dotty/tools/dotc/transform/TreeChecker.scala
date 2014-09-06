package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Flags._
import core.Constants._
import core.StdNames._
import core.Decorators._
import core.TypeErasure.isErasedType
import typer._
import typer.ErrorReporting._
import reporting.ThrowingReporter
import ast.Trees._
import ast.{tpd, untpd}
import util.SourcePosition
import java.lang.AssertionError

/** Run by -Ycheck option after a given phase, this class retypes all syntax trees
 *  and verifies that the type of each tree node so obtained conforms to the type found in the tree node.
 *  It also performs the following checks:
 *
 *   - The owner of each definition is the same as the owner of the current typing context.
 *   - Ident nodes do not refer to a denotation that would need a select to be accessible
 *     (see tpd.needsSelect).
 *   - After typer, identifiers and select nodes refer to terms only (all types should be
 *     represented as TypeTrees then).
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
    override def typed(tree: untpd.Tree, pt: Type)(implicit ctx: Context) = {
      val res = tree match {
        case _: untpd.UnApply =>
          // can't recheck patterns
          tree.asInstanceOf[tpd.Tree]
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[_] =>
          super.typed(tree)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typed(tree, pt)
          def isSubType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 <:< tp2)
          def divergenceMsg(tp1: Type, tp2: Type) =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
               |Why different :
             """.stripMargin + core.TypeComparer.explained((tp1 <:< tp2)(_))
          assert(isSubType(tree1.tpe, tree.typeOpt), divergenceMsg(tree1.tpe, tree.typeOpt))
          tree1
        }
      if (ctx.erasedTypes) {
        assertErased(res)
        res match {
          case res: This =>
            assert(!ExplicitOuter.referencesOuter(ctx.owner.enclosingClass, res),
              i"Reference to $res from ${ctx.owner.showLocated}")
          case _ =>
        }
      }
      res
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      assert(tree.isType || !needsSelect(tree.tpe), i"bad type ${tree.tpe} for $tree")
      super.typedIdent(tree, pt)
    }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      super.typedSelect(tree, pt)
    }

    private def checkOwner(tree: untpd.Tree)(implicit ctx: Context): Unit = {
      def ownerMatches(symOwner: Symbol, ctxOwner: Symbol): Boolean =
        symOwner == ctxOwner ||
        ctxOwner.isTerm && !(ctxOwner is Method | Lazy | Mutable) &&
          ownerMatches(symOwner, ctxOwner.owner)
      assert(ownerMatches(tree.symbol.owner, ctx.owner),
             i"bad owner; ${tree.symbol} has owner ${tree.symbol.owner}, expected was ${ctx.owner}")
    }

    override def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = {
      val TypeDef(_, _, impl @ Template(constr, _, _, _)) = cdef
      checkOwner(impl)
      checkOwner(impl.constr)
      super.typedClassDef(cdef, cls)
    }

    /** Check that all defined symbols have legal owners.
     *  An owner is legal if it is either the same as the context's owner
     *  or there's an owner chain of valdefs starting at the context's owner and
     *  reaching up to the symbol's owner. The reason for this relaxed matching
     *  is that we should be able to pull out an expression as an initializer
     *  of a helper value without having to do a change owner traversal of the expression.
     */
    override def index(trees: List[untpd.Tree])(implicit ctx: Context): Context = {
      for (tree <- trees if tree.isDef) checkOwner(tree)
      super.index(trees)
    }

    override def adapt(tree: Tree, pt: Type, original: untpd.Tree = untpd.EmptyTree)(implicit ctx: Context) = {
      if (ctx.mode.isExpr)
        assert(tree.tpe <:< pt,
            s"error at ${sourcePos(tree.pos)}\n" +
            err.typeMismatchStr(tree.tpe, pt))
      tree
    }
  }

  def assertErased(tp: Type, tree: Tree = EmptyTree)(implicit ctx: Context): Unit =
    assert(isErasedType(tp), i"The type $tp - ${tp.toString} of class ${tp.getClass} of tree $tree / ${tree.getClass} is illegal after erasure, phase = ${ctx.phase}")

  /** Assert that tree type and its widened underlying type are erased.
   *  Also assert that term refs have fixed symbols (so we are sure
   *  they need not be reloaded using member; this would likely fail as signatures
   *  may change after erasure).
   */
  def assertErased(tree: Tree)(implicit ctx: Context): Unit = {
    assertErased(tree.typeOpt, tree)
    if (!(tree.symbol == defn.Any_isInstanceOf || tree.symbol == defn.Any_asInstanceOf))
      assertErased(tree.typeOpt.widen, tree)
    if (ctx.mode.isExpr)
      tree.tpe match {
        case ref: TermRef =>
          assert(ref.denot.isInstanceOf[SymDenotation],
            i"non-sym type $ref of class ${ref.getClass} with denot of class ${ref.denot.getClass} of $tree")
        case _ =>
      }
  }
}

object TreeChecker extends TreeChecker