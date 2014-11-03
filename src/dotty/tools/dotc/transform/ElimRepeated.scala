package dotty.tools.dotc
package transform

import core._
import Names._
import Types._
import TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees.flatten
import Flags._
import Contexts.Context
import Symbols._
import Denotations._, SymDenotations._
import Decorators.StringInterpolators
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import scala.collection.mutable
import DenotTransformers._
import Names.Name
import NameOps._
import TypeUtils._

/** A transformer that removes repeated parameters (T*) from all types, replacing
 *  them with Seq types.
 */
class ElimRepeated extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "elimRepeated"

  object annotTransformer extends TreeMap {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = super.transform(tree) match {
        case x @(_: Ident|_ :Select|_: Apply| _: TypeApply| _: DefDef) => transformTypeOfTree(x)
        case x => x
      }
  }

  /**
   * Overriden to solve a particular problem with <repeated> not being eliminated inside annotation trees
   * Dmitry: this should solve problem for now,
   *         following YAGNI principle I am convinced that we shouldn't make a solution
   *         for a generalized problem(transforming annotations trees)
   *         that manifests itself only here.
   */
  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val info1 = transformInfo(ref.info, ref.symbol)

    ref match {
      case ref: SymDenotation =>
        val annotTrees = ref.annotations.map(_.tree)
        val annotTrees1 = annotTrees.mapConserve(annotTransformer.transform)
        val annots1 = if(annotTrees eq annotTrees1) ref.annotations else annotTrees1.map(new ConcreteAnnotation(_))
        if ((info1 eq ref.info) && (annots1 eq ref.annotations)) ref
        else ref.copySymDenotation(info = info1, annotations = annots1)
      case _ => if (info1 eq ref.info) ref else ref.derivedSingleDenotation(ref.symbol, info1)
    }
  }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimRepeated(tp)

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

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  /** If method overrides a Java varargs method, add a varargs bridge.
   */
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    assert(ctx.phase == thisTransformer)
    def overridesJava = tree.symbol.allOverriddenSymbols.exists(_ is JavaDefined)
    val newAnnots = tree.mods.annotations.mapConserve(annotTransformer.transform)
    val newTree = if (newAnnots eq tree.mods.annotations) tree
      else cpy.DefDef(tree)(mods  = Modifiers(tree.mods.flags, tree.mods.privateWithin, newAnnots))
    if (tree.symbol.info.isVarArgsMethod && overridesJava)
      addVarArgsBridge(newTree)(ctx.withPhase(thisTransformer.next))
    else
      newTree
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
