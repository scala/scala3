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
class ReifyQuotes extends MacroTransform {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.containsQuotes) super.run

  protected def newTransformer(implicit ctx: Context): Transformer = new Reifier

  /** Serialize `tree`. Embedded splices are represented as nodes of the form
   *
   *      Select(qual, sym)
   *
   *  where `sym` is either `defn.MetaExpr_~` or `defn.MetaType_~`. For any splice,
   *  the `qual` part should not be pickled, since it will be added separately later
   *  as a splice.
   */
  def pickleTree(tree: Tree, isType: Boolean)(implicit ctx: Context): String =
    tree.show // TODO: replace with TASTY

  private class Reifier extends Transformer {

    /** Turn `body` of quote into a call of `scala.meta.Unpickler.unpickleType` or
     *  `scala.meta.Unpickler.unpickleExpr` depending onwhether `isType` is true or not.
     *  The arguments to the method are:
     *
     *    - the serialized `body`, as returned from `pickleTree`
     *    - all splices found in `body`
     */
    private def reifyCall(body: Tree, isType: Boolean)(implicit ctx: Context) = {

      object collectSplices extends TreeAccumulator[mutable.ListBuffer[Tree]] {
        override def apply(splices: mutable.ListBuffer[Tree], tree: Tree)(implicit ctx: Context) = tree match {
          case tree @ Select(qual, _)
          if tree.symbol == defn.MetaExpr_~ || tree.symbol == defn.MetaType_~ =>
            splices += transform(qual)
          case _ =>
            foldOver(splices, tree)
        }
      }
      val splices = collectSplices(new mutable.ListBuffer[Tree], body).toList
      val reified = pickleTree(body, isType)

      ref(if (isType) defn.Unpickler_unpickleType else defn.Unpickler_unpickleExpr)
        .appliedToType(if (isType) body.tpe else body.tpe.widen)
        .appliedTo(
          Literal(Constant(reified)),
          SeqLiteral(splices, TypeTree(defn.MetaQuotedType)))
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Apply(fn, arg :: Nil) if fn.symbol == defn.quoteMethod =>
        reifyCall(arg, isType = false)
      case TypeApply(fn, arg :: Nil) if fn.symbol == defn.typeQuoteMethod =>
        reifyCall(arg, isType = true)
      case _ =>
        super.transform(tree)
    }
  }
}