package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Types._
import ast.Trees._
import ast.tpd.{Apply, Tree, cpy}
import dotty.tools.dotc.ast.tpd
import scala.collection.mutable
import dotty.tools.dotc._
import core._
import Contexts._
import Symbols._
import Decorators._
import NameOps._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransformer, TreeTransform}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{untpd, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Names.Name
import dotty.runtime.LazyVals
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import StdNames._
import Phases.Phase

/** Replace member references as follows:
  *
  * - `x != y` for != in class Any becomes `!(x == y)` with == in class Any.
  * - `x.##` for ## in NullClass becomes `0`
  * - `x.##` for ## in Any becomes calls to ScalaRunTime.hash,
  *     using the most precise overload available
  * - `x.getClass` for getClass in primitives becomes `x.getClass` with getClass in class Object.
  */
class InterceptedMethods extends MiniPhaseTransform {
  thisTransform =>

  import tpd._

  override def phaseName: String = "intercepted"

  private var primitiveGetClassMethods: Set[Symbol] = _

  var Any_## : Symbol = _ // cached for performance reason

  /** perform context-dependant initialization */
  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    this.Any_## = defn.Any_##
    primitiveGetClassMethods = Set[Symbol]() ++ defn.ScalaValueClasses().map(x => x.requiredMethod(nme.getClass_))
    this
  }

  // this should be removed if we have guarantee that ## will get Apply node
  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.symbol.isTerm && (Any_## eq tree.symbol.asTerm)) {
      val rewrite = poundPoundValue(tree.qualifier)
      ctx.log(s"$phaseName rewrote $tree to $rewrite")
      rewrite
    }
    else tree
  }

  private def poundPoundValue(tree: Tree)(implicit ctx: Context) = {
    val s = tree.tpe.widen.typeSymbol
    if (s == defn.NullClass) Literal(Constant(0))
    else {
      // Since we are past typer, we need to avoid creating trees carrying
      // overloaded types.  This logic is custom (and technically incomplete,
      // although serviceable) for def hash.  What is really needed is for
      // the overloading logic presently hidden away in a few different
      // places to be properly exposed so we can just call "resolveOverload"
      // after typer.  Until then:

      def alts = defn.ScalaRuntimeModule.info.member(nme.hash_)

      // if tpe is a primitive value type, alt1 will match on the exact value,
      // taking in account that null.asInstanceOf[Int] == 0
      def alt1 = alts.suchThat(_.info.firstParamTypes.head =:= tree.tpe.widen)

      // otherwise alt2 will match. alt2 also knows how to handle 'null' runtime value
      def alt2 = defn.ScalaRuntimeModule.info.member(nme.hash_)
        .suchThat(_.info.firstParamTypes.head.typeSymbol == defn.AnyClass)

      Ident((if (s.isNumericValueClass) alt1 else alt2).termRef)
        .appliedTo(tree)
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    def unknown = {
      assert(false, s"The symbol '${tree.fun.symbol.showLocated}' was intercepted but didn't match any cases, " +
        s"that means the intercepted methods set doesn't match the code")
      tree
    }
    lazy val Select(qual, _) = tree.fun
    val Any_## = this.Any_##
    val Any_!= = defn.Any_!=
    val rewrite: Tree = tree.fun.symbol match {
      case Any_## =>
          poundPoundValue(qual)
      case Any_!= =>
          qual.select(defn.Any_==).appliedToArgs(tree.args).select(defn.Boolean_!)
        /*
        /* else if (isPrimitiveValueClass(qual.tpe.typeSymbol)) {
            // todo: this is needed to support value classes
            // Rewrite 5.getClass to ScalaRunTime.anyValClass(5)
            global.typer.typed(gen.mkRuntimeCall(nme.anyValClass,
              List(qual, typer.resolveClassTag(tree.pos, qual.tpe.widen))))
          }*/
         */
      case t if primitiveGetClassMethods.contains(t) =>
          // if we got here then we're trying to send a primitive getClass method to either
          // a) an Any, in which cage Object_getClass works because Any erases to object. Or
          //
          // b) a non-primitive, e.g. because the qualifier's type is a refinement type where one parent
          //    of the refinement is a primitive and another is AnyRef. In that case
          //    we get a primitive form of _getClass trying to target a boxed value
          //    so we need replace that method name with Object_getClass to get correct behavior.
          //    See SI-5568.
          qual.selectWithSig(defn.Any_getClass).appliedToNone
      case _ =>
        tree
    }
    ctx.log(s"$phaseName rewrote $tree to $rewrite")
    rewrite
  }
}
