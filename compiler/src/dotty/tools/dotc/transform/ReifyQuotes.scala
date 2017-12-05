package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import MegaPhase.MiniPhase
import scala.collection.mutable

/** Translates quoted terms and types to reify method calls.
 *  The mini phase needs to run in the same group as FirstTransform. So far we lack
 *  the machinery to express this constraint in code.
 */
class ReifyQuotes extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  /** Serialize `tree`. Embedded splices are represented as nodes of the form
   *
   *      Select(qual, sym)
   *
   *  where `sym` is either `defn.MetaExpr_~` or `defn.MetaType_~`. For any splice,
   *  the `qual` part will be of the form `Typed(EmptyTree, TypeTree(<underlying type>))`,
   *  but that knowledge is not needed to uniquely identify a splice node.
   */
  def pickleTree(tree: Tree, isType: Boolean)(implicit ctx: Context): String =
    tree.show // TODO: replace with TASTY

  private def reifyCall(body: Tree, isType: Boolean)(implicit ctx: Context) = {

    object liftSplices extends TreeMap {
      val splices = new mutable.ListBuffer[Tree]
      override def transform(tree: Tree)(implicit ctx: Context) = tree match {
        case tree @ Select(qual, name)
        if tree.symbol == defn.MetaExpr_~ || tree.symbol == defn.MetaType_~ =>
          splices += {
            if (isType) // transform splice again because embedded type trees were not rewritten before
              transformAllDeep(qual)(ctx.retractMode(Mode.InQuotedType))
            else qual
          }
          val placeHolder = Typed(EmptyTree, TypeTree(qual.tpe.widen))
          cpy.Select(tree)(placeHolder, name)
        case _ =>
          super.transform(tree)
      }
    }

    val reified = pickleTree(liftSplices.transform(body), isType)
    val splices = liftSplices.splices.toList
    val spliceType = if (isType) defn.MetaTypeType else defn.MetaExprType

    ref(if (isType) defn.Unpickler_unpickleType else defn.Unpickler_unpickleExpr)
      .appliedToType(if (isType) body.tpe else body.tpe.widen)
      .appliedTo(
        Literal(Constant(reified)),
        SeqLiteral(splices, TypeTree(spliceType.appliedTo(TypeBounds.empty))))
  }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    if (tree.fun.symbol == defn.quoteMethod) reifyCall(tree.args.head, isType = false)
    else tree

  override def prepareForTypeApply(tree: TypeApply)(implicit ctx: Context): Context =
    if (tree.symbol == defn.typeQuoteMethod) ctx.addMode(Mode.InQuotedType) else ctx

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree =
    if (tree.fun.symbol == defn.typeQuoteMethod) reifyCall(tree.args.head, isType = true)
    else tree
}