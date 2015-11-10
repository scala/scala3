package dotty.tools.dotc
package transform

import core._
import Names._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.TreeTransforms.{AnnotationTransformer, TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Contexts.Context
import Symbols._
import Constants._
import Denotations._, SymDenotations._
import Decorators.StringInterpolators
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import scala.collection.mutable
import DenotTransformers._
import Names.Name
import NameOps._
import TypeUtils._

/** A transformer that removes repeated parameters (T*) from all types, replacing
 *  them with Seq types.
 */
class ElimRepeated extends MiniPhaseTransform with InfoTransformer with AnnotationTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "elimRepeated"

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimRepeated(tp)

  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym is Method

  private def elimRepeated(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
    case tp @ MethodType(paramNames, paramTypes) =>
      val resultType1 = elimRepeated(tp.resultType)
      val paramTypes1 =
        if (paramTypes.nonEmpty && paramTypes.last.isRepeatedParam) {
          val last = paramTypes.last.underlyingIfRepeated(tp.isJava)
          paramTypes.init :+ last
        } else paramTypes
      tp.derivedMethodType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, elimRepeated(tp.resultType))
    case tp =>
      tp
  }

  def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimRepeated(tree.tpe))

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val args1 = tree.args.map {
      case arg: Typed if isWildcardStarArg(arg) =>
        if (tree.fun.symbol.is(JavaDefined) && arg.expr.tpe.derivesFrom(defn.SeqClass))
          seqToArray(arg.expr)
        else arg.expr
      case arg => arg
    }
    transformTypeOfTree(cpy.Apply(tree)(tree.fun, args1))
  }

  /** Convert sequence argument to Java array */
  private def seqToArray(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case SeqLiteral(elems) =>
      JavaSeqLiteral(elems)
    case _ =>
      val elemType = tree.tpe.firstBaseArgInfo(defn.SeqClass)
      var elemClass = elemType.classSymbol
      if (defn.PhantomClasses contains elemClass) elemClass = defn.ObjectClass
      ref(defn.DottyArraysModule)
        .select(nme.seqToArray)
        .appliedToType(elemType)
        .appliedTo(tree, Literal(Constant(elemClass.typeRef)))
        .ensureConforms(defn.ArrayOf(elemType))
          // Because of phantomclasses, the Java array's type might not conform to the return type
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  /** If method overrides a Java varargs method, add a varargs bridge.
   *  Also transform trees inside method annotation
   */
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    assert(ctx.phase == thisTransformer)
    def overridesJava = tree.symbol.allOverriddenSymbols.exists(_ is JavaDefined)
    if (tree.symbol.info.isVarArgsMethod && overridesJava)
        addVarArgsBridge(tree)(ctx.withPhase(thisTransformer.next))
     else
      tree
  }

  /** Add a Java varargs bridge
   *  @param  ddef  the original method definition which is assumed to override
   *                a Java varargs method JM up to this phase.
   *  @return  a thicket consisting of `ddef` and a varargs bridge method
   *           which overrides the Java varargs method JM from this phase on
   *           and forwards to `ddef`.
   */
  private def addVarArgsBridge(ddef: DefDef)(implicit ctx: Context): Tree = {
    val original = ddef.symbol.asTerm
    val bridge = original.copy(
      flags = ddef.symbol.flags &~ Private | Artifact,
      info = toJavaVarArgs(ddef.symbol.info)).enteredAfter(thisTransformer).asTerm
    val bridgeDef = polyDefDef(bridge, trefs => vrefss => {
      val (vrefs :+ varArgRef) :: vrefss1 = vrefss
      val elemtp = varArgRef.tpe.widen.argTypes.head
      ref(original.termRef)
        .appliedToTypes(trefs)
        .appliedToArgs(vrefs :+ TreeGen.wrapArray(varArgRef, elemtp))
        .appliedToArgss(vrefss1)
    })
    Thicket(ddef, bridgeDef)
  }

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: PolyType =>
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      val inits :+ last = tp.paramTypes
      val last1 = last.underlyingIfRepeated(isJava = true)
      tp.derivedMethodType(tp.paramNames, inits :+ last1, tp.resultType)
  }
}
