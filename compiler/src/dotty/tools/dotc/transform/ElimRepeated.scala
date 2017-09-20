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
import Decorators._
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

  override def changesMembers = true // the phase adds vararg bridges

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimRepeated(tp)

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
    super.transform(ref) match {
      case ref1: SymDenotation if (ref1 ne ref) && overridesJava(ref1.symbol) =>
        // This method won't override the corresponding Java method at the end of this phase,
        // only the bridge added by `addVarArgsBridge` will.
        ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
      case ref1 =>
        ref1
    }

  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym is Method

  private def overridesJava(sym: Symbol)(implicit ctx: Context) = sym.allOverriddenSymbols.exists(_ is JavaDefined)

  private def elimRepeated(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType)
      val paramTypes1 =
        if (paramTypes.nonEmpty && paramTypes.last.isRepeatedParam) {
          val last = paramTypes.last.underlyingIfRepeated(tp.isJava)
          paramTypes.init :+ last
        } else paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType))
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
    val formals = (tree.fun.tpe.widen: @unchecked) match {
      case mt: MethodType => mt.paramInfos
    }
    val args1 = tree.args.zipWithConserve(formals) { (arg, formal) =>
      arg match {
        case arg: Typed if isWildcardStarArg(arg) =>
          if (tree.fun.symbol.is(JavaDefined) && arg.expr.tpe.derivesFrom(defn.SeqClass))
            seqToArray(arg.expr, formal.translateParameterized(defn.RepeatedParamClass, defn.ArrayClass))
          else arg.expr
        case arg => arg
      }
    }
    transformTypeOfTree(cpy.Apply(tree)(tree.fun, args1))
  }

  /** Convert sequence argument to Java array of type `pt` */
  private def seqToArray(tree: Tree, pt: Type)(implicit ctx: Context): Tree = tree match {
    case SeqLiteral(elems, elemtpt) =>
      JavaSeqLiteral(elems, elemtpt)
    case _ =>
      val elemType = tree.tpe.elemType
      var elemClass = elemType.classSymbol
      if (defn.NotRuntimeClasses contains elemClass) elemClass = defn.ObjectClass
      ref(defn.DottyArraysModule)
        .select(nme.seqToArray)
        .appliedToType(elemType)
        .appliedTo(tree, Literal(Constant(elemClass.typeRef)))
        .ensureConforms(pt)
          // Because of phantomclasses, the Java array's type might not conform to the return type
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  /** If method overrides a Java varargs method, add a varargs bridge.
   *  Also transform trees inside method annotation
   */
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    assert(ctx.phase == thisTransformer)
    if (tree.symbol.info.isVarArgsMethod && overridesJava(tree.symbol))
      addVarArgsBridge(tree)
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
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      val inits :+ last = tp.paramInfos
      val last1 = last.underlyingIfRepeated(isJava = true)
      tp.derivedLambdaType(tp.paramNames, inits :+ last1, tp.resultType)
  }
}
