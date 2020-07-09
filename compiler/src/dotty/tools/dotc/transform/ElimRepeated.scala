package dotty.tools.dotc
package transform

import core._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import Flags._
import Contexts._
import Symbols._
import Constants._
import Decorators._
import Denotations._, SymDenotations._
import dotty.tools.dotc.ast.tpd
import TypeErasure.erasure
import DenotTransformers._

object ElimRepeated {
  val name: String = "elimRepeated"
}

/** A transformer that removes repeated parameters (T*) from all types, replacing
 *  them with Seq types.
 */
class ElimRepeated extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ElimRepeated.name

  override def changesMembers: Boolean = true // the phase adds vararg bridges

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    elimRepeated(tp)

  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    super.transform(ref) match
      case ref1: SymDenotation if (ref1 ne ref) && overridesJava(ref1.symbol) =>
        // This method won't override the corresponding Java method at the end of this phase,
        // only the bridge added by `addVarArgsBridge` will.
        ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
      case ref1 =>
        ref1

  override def mayChange(sym: Symbol)(using Context): Boolean = sym.is(Method)

  private def overridesJava(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(_.is(JavaDefined))

  private def hasVarargsAnnotation(sym: Symbol)(using Context) = sym.hasAnnotation(defn.VarargsAnnot)

  private def parentHasAnnotation(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(hasVarargsAnnotation)

  /** Eliminate repeated parameters from method types. */
  private def elimRepeated(tp: Type)(using Context): Type = tp.stripTypeVar match
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType)
      val paramTypes1 =
        if paramTypes.nonEmpty && paramTypes.last.isRepeatedParam then
          val last = paramTypes.last.translateFromRepeated(toArray = tp.isJavaMethod)
          paramTypes.init :+ last
        else paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType))
    case tp =>
      tp

  def transformTypeOfTree(tree: Tree)(using Context): Tree =
    tree.withType(elimRepeated(tree.tpe))

  override def transformTypeApply(tree: TypeApply)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformIdent(tree: Ident)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(using Context): Tree =
    val args = tree.args.mapConserve {
      case arg: Typed if isWildcardStarArg(arg) =>
        val isJavaDefined = tree.fun.symbol.is(JavaDefined)
        val tpe = arg.expr.tpe
        if isJavaDefined && tpe.derivesFrom(defn.SeqClass) then
          seqToArray(arg.expr)
        else if !isJavaDefined && tpe.derivesFrom(defn.ArrayClass)
          arrayToSeq(arg.expr)
        else
          arg.expr
      case arg => arg
    }
    transformTypeOfTree(cpy.Apply(tree)(tree.fun, args))

  /** Convert sequence argument to Java array */
  private def seqToArray(tree: Tree)(using Context): Tree = tree match
    case SeqLiteral(elems, elemtpt) =>
      JavaSeqLiteral(elems, elemtpt)
    case _ =>
      val elemType = tree.tpe.elemType
      var elemClass = erasure(elemType).classSymbol
      if defn.NotRuntimeClasses.contains(elemClass) then
        elemClass = defn.ObjectClass
      end if
      ref(defn.DottyArraysModule)
        .select(nme.seqToArray)
        .appliedToType(elemType)
        .appliedTo(tree, clsOf(elemClass.typeRef))

  /** Convert Java array argument to Scala Seq */
  private def arrayToSeq(tree: Tree)(using Context): Tree =
    tpd.wrapArray(tree, tree.tpe.elemType)

  /** If method overrides a Java varargs method or is annotated with @varargs, add a varargs bridge.
   *  Also transform trees inside method annotation.
   */
  override def transformDefDef(tree: DefDef)(using Context): Tree =
    atPhase(thisPhase) {
      val sym = tree.symbol
      val hasAnnotation = hasVarargsAnnotation(sym)
      if hasRepeatedParams(sym) then
        val isOverride = overridesJava(sym)
        if isOverride || hasAnnotation || parentHasAnnotation(sym) then
          // java varargs are more restrictive than scala's
          // see https://github.com/scala/bug/issues/11714
          if !isValidJavaVarArgs(sym.info) then
            ctx.error("""To generate java-compatible varargs:
                      |  - there must be a single repeated parameter
                      |  - it must be the last argument in the last parameter list
                      |""".stripMargin,
              sym.sourcePos)
            tree
          else
            addVarArgsBridge(tree, isOverride)
        else
          tree
      else
        if hasAnnotation then
          ctx.error("A method without repeated parameters cannot be annotated with @varargs", sym.sourcePos)
        tree
    }

  /** Is there a repeated parameter in some parameter list? */
  private def hasRepeatedParams(sym: Symbol)(using Context): Boolean =
    sym.info.paramInfoss.flatten.exists(_.isRepeatedParam)

  /** Is this the type of a method that has a repeated parameter type as
   *  its last parameter in the last parameter list?
   */
  private def isValidJavaVarArgs(tp: Type)(using Context): Boolean = tp match
    case mt: MethodType =>
      val initp :+ lastp = mt.paramInfoss
      initp.forall(_.forall(!_.isRepeatedParam)) &&
      lastp.nonEmpty &&
      lastp.init.forall(!_.isRepeatedParam) &&
      lastp.last.isRepeatedParam
    case pt: PolyType =>
      isValidJavaVarArgs(pt.resultType)
    case _ =>
      throw new Exception("Match error in @varargs bridge logic. This should not happen, please open an issue " + tp)


  /** Add a Java varargs bridge
   *  @param ddef    the original method definition
   *  @param addFlag the flag to add to the method symbol

   *  @return a thicket consisting of `ddef` and a varargs bridge method
   *          which forwards java varargs to `ddef`. It retains all the
   *          flags of `ddef` except `Private`.
   *
   *  A bridge is necessary because the following hold:
   *    - the varargs in `ddef` will change from `RepeatedParam[T]` to `Seq[T]` after this phase
   *    - _but_ the callers of `ddef` expect its varargs to be changed to `Array[? <: T]`
   *  The solution is to add a "bridge" method that converts its argument from `Array[? <: T]` to `Seq[T]` and
   *  forwards it to `ddef`.
   */
  private def addVarArgsBridge(ddef: DefDef, javaOverride: Boolean)(using ctx: Context): Tree =
    val original = ddef.symbol.asTerm
    // For simplicity we always set the varargs flag
    // although it's not strictly necessary for overrides
    // (but it is for non-overrides)
    val flags = ddef.symbol.flags | JavaVarargs

    // The java-compatible bridge symbol
    val bridge = original.copy(
        // non-overrides cannot be synthetic otherwise javac refuses to call them
        flags = if javaOverride then flags | Artifact else flags,
        info = toJavaVarArgs(ddef.symbol.info)
      ).asTerm

    currentClass.info.member(bridge.name).alternatives.find { s =>
      s.matches(bridge) &&
      !(javaOverride && s.asSymDenotation.is(JavaDefined))
    } match
      case Some(conflict) =>
        ctx.error(s"@varargs produces a forwarder method that conflicts with ${conflict.showDcl}", original.sourcePos)
        ddef
      case None =>
        val bridgeDef = polyDefDef(bridge.enteredAfter(thisPhase), trefs => vrefss => {
          val init :+ (last :+ vararg) = vrefss
          // Can't call `.argTypes` here because the underlying array type is of the
          // form `Array[? <: SomeType]`, so we need `.argInfos` to get the `TypeBounds`.
          val elemtp = vararg.tpe.widen.argInfos.head

          // The generation of the forwarding call needs to be deferred, otherwise
          // generic and curried methods won't pass the tree checker.
          atNextPhase {
            ref(original.termRef)
              .appliedToTypes(trefs)
              .appliedToArgss(init)
              .appliedToArgs(last :+ tpd.wrapArray(vararg, elemtp))
            }
          })
        Thicket(ddef, bridgeDef)

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(using Context): Type = tp match
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      tp.resultType match
        case m: MethodType => // multiple param lists
          tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(m))
        case _ =>
          val init :+ last = tp.paramInfos
          val vararg = varargArrayType(last)
          tp.derivedLambdaType(tp.paramNames, init :+ vararg, tp.resultType)

  /** Translate a repeated type T* to an `Array[? <: Upper]`
   *  such that it is compatible with java varargs.
   *
   *  When necessary we set `Upper = T & AnyRef`
   *  to prevent the erasure of `Array[? <: Upper]` to Object,
   *  which would break the varargs from Java.
   */
  private def varargArrayType(tp: Type)(using Context): Type =
    val array = tp.translateFromRepeated(toArray = true) // Array[? <: T]
    val element = array.elemType.hiBound // T

    if element <:< defn.AnyRefType || element.typeSymbol.isPrimitiveValueClass then array
    else defn.ArrayOf(TypeBounds.upper(AndType(element, defn.AnyRefType))) // Array[? <: T & AnyRef]
}
