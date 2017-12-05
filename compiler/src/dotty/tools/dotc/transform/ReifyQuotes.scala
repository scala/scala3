package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import MegaPhase.MiniPhase
import scala.collection.mutable

/** Translates quoted terms and types to reify method calls.
 */
class ReifyQuotes extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  def reifyTree(tree: Tree)(implicit ctx: Context): String = i"($tree)" // TODO: replace with TASTY
  def reifyType(tpe: Type)(implicit ctx: Context): String = i"[$tpe]"   // TODO: replace with TASTY

  private def reifyCall(meth: Symbol, typeArg: Type, reified: String, splices: List[Tree])(implicit ctx: Context) =
    ref(meth)
      .appliedToType(typeArg)
      .appliedTo(
        Literal(Constant(reified)),
        SeqLiteral(splices, TypeTree(defn.MetaExprType.appliedTo(TypeBounds.empty))))

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    object liftSplices extends TreeMap {
      val splices = new mutable.ListBuffer[Tree]
      override def transform(tree: Tree)(implicit ctx: Context) = tree match {
        case tree @ Select(qual, name) if tree.symbol == defn.MetaExpr_~ =>
          splices += qual
          val placeHolder = Typed(EmptyTree, TypeTree(qual.tpe.widen))
          cpy.Select(tree)(placeHolder, name)
        case _ =>
          super.transform(tree)
      }
    }
    if (tree.fun.symbol == defn.quoteMethod) {
      val body = tree.args.head
      val reified = reifyTree(liftSplices.transform(body))
      reifyCall(defn.Reifier_reifyExpr, body.tpe.widen, reified, liftSplices.splices.toList)
    }
    else tree
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = {
    object liftSplices extends TypeMap {
      val splices = new mutable.ListBuffer[Tree]
      override def apply(t: Type) = t match {
        case t @ TypeRef(pre: TermRef, _) if t.symbol == defn.MetaType_~ =>
          splices += ref(pre).withPos(tree.args.head.pos)
          val placeHolder = SkolemType(pre.widen)
          t.withPrefix(placeHolder)
        case _ =>
          mapOver(t)
      }
    }
    if (tree.fun.symbol == defn.typeQuoteMethod) {
      val body = tree.args.head.tpe
      val reified = reifyType(liftSplices(body))
      reifyCall(defn.Reifier_reifyType, body, reified, liftSplices.splices.toList)
    }
    else tree
  }
}
