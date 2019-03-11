package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._
import Contexts._, Trees._, Types._
import DenotTransformers._, MegaPhase._
import ExtensionMethods._, ValueClasses._


/** This phase inlines calls to methods of value classes.
 *
 *  A value class V after [[ExtensionMethods]] will look like:
 *    class V[A, B, ...](val underlying: U) extends AnyVal {
 *      def foo[T, S, ...](arg1: A1, arg2: A2, ...) =
 *        V.foo$extension[T, S, ..., A, B, ...](this)(arg1, arg2, ...)
 *
 *       ...
 *    }
 *
 *  Let e have type V, if e is a stable prefix or if V does not have any class
 *  type parameter, then we can rewrite:
 *    e.foo[X, Y, ...](args)
 *  as:
 *    V.foo$extension[X, Y, ..., A', B', ...](e)(args)
 *    where A', B', ... are the class type parameters A, B, ... as seen from `e`.
 *  Otherwise, we need to evaluate e first:
 *    {
 *      val ev = e
 *      V.foo$extension[X, Y, ..., A', B', ...](ev)(args)
 *    }
 *
 *  This phase needs to be placed after phases which may introduce calls to
 *  value class methods (like [[PatternMatcher]]). This phase uses name mangling
 *  to find the correct extension method corresponding to a value class method
 *  (see [[ExtensionMethods.extensionMethod]]), therefore we choose to place it
 *  before phases which may perform their own name mangling on value class
 *  methods (like [[TypeSpecializer]]), this way [[VCInlineMethods]] does not
 *  need to have any knowledge of the name mangling done by other phases.
 */
class VCInlineMethods extends MiniPhase with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "vcInlineMethods"

  override def runsAfter: Set[String] =
    Set(ExtensionMethods.name, PatternMatcher.name)

  /** Replace a value class method call by a call to the corresponding extension method.
   *
   *  @param tree   The tree corresponding to the method call
   *  @param mtArgs Type arguments for the method call not present in `tree`
   *  @param mArgss Arguments for the method call not present in `tree`
   *  @return       A tree for the extension method call
   */
  private def rewire(tree: Tree, mtArgs: List[Tree] = Nil, mArgss: List[List[Tree]] = Nil)
    (implicit ctx: Context): Tree =
    tree match {
      case Apply(qual, mArgs) =>
        rewire(qual, mtArgs, mArgs :: mArgss)
      case TypeApply(qual, mtArgs2) =>
        assert(mtArgs == Nil)
        rewire(qual, mtArgs2, mArgss)
      case sel @ Select(qual, _) =>
        val origMeth = sel.symbol
        val origCls = origMeth.enclosingClass
        val ctParams = origCls.typeParams
        val extensionMeth = extensionMethod(origMeth)

        if (!ctParams.isEmpty) {
          evalOnce(qual) { ev =>
            val ctArgs = ctParams.map(tparam =>
              TypeTree(tparam.typeRef.asSeenFrom(ev.tpe, origCls)))
            ref(extensionMeth)
              .appliedToTypeTrees(mtArgs ++ ctArgs)
              .appliedTo(ev)
              .appliedToArgss(mArgss)
          }
        } else {
          ref(extensionMeth)
            .appliedToTypeTrees(mtArgs)
            .appliedTo(qual)
            .appliedToArgss(mArgss)
        }
    }

  /** If this tree corresponds to a fully-applied value class method call, replace it
   *  by a call to the corresponding extension method, otherwise return it as is.
   */
  private def rewireIfNeeded(tree: Tree)(implicit ctx: Context) = tree.tpe.widen match {
    case tp: LambdaType =>
      tree // The rewiring will be handled by a fully-applied parent node
    case _ =>
      if (isMethodWithExtension(tree.symbol))
        rewire(tree).ensureConforms(tree.tpe).withSpan(tree.span)
      else
        tree
  }

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    rewireIfNeeded(tree)
  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree =
    rewireIfNeeded(tree)
  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    rewireIfNeeded(tree)
}
