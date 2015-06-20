package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Names._, Trees._, Types._, StdNames._, Symbols._, Flags._
import DenotTransformers._, TreeTransforms._, Phases.Phase
import ExtensionMethods._, TreeExtractors._, ValueClasses._, TreeExtractors._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._

import scala.collection.mutable.ListBuffer

/** This phase adds overloaded `==` and `!=` methods to value classes.
 *
 *  For a value class V defined as:
 *    class V(val underlying: U) extends AnyVal
 *  We want equality comparisons to avoid boxing as much as possible. This is
 *  achieved by synthesizing the following method (unless it already exists):
 *    def ==(x$0: V): Boolean
 *  This way, we will get an extension method for `==` whose signature does not
 *  force boxing:
 *    def ==$extension($this: U, x$0: U): Boolean
 *
 *  We do the same for `!=`.
 */
class VCEqMethods extends MiniPhaseTransform with DenotTransformer {
  import tpd._

  override def phaseName: String = "vcEqMethods"

  private val eqNames = List(nme.EQ, nme.NE)

  /** For a value class V, we create symbols for the following methods if they don't exist:
   *    def ==(x$0: V): Boolean
   *    def !=(x$0: V): Boolean
   *  Unless V was compiled by Scala 2.
   */
  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case vc: ClassDenotation if isDerivedValueClass(vc) && !vc.is(Scala2x) =>
      val cinfo = vc.classInfo
      val decls1 = cinfo.decls.cloneScope
      eqNames foreach { eqName =>
        if (!vcEqSym(vc, eqName).exists) {
          val eqSym = ctx.newSymbol(vc.symbol, eqName, Synthetic | Method,
            MethodType(List(nme.x_0), List(vc.typeRef), defn.BooleanType))
          decls1.enter(eqSym)
        }
      }
      vc.copySymDenotation(info = cinfo.derivedClassInfo(decls = decls1))
    case _ =>
      ref
  }

  /** For a value class V, return (if it exists) the symbol for the method:
   *    def `eqName`(x$0: V): Boolean
   *
   *  @param vc      The denotation of the value class V
   *  @param eqName  The name of the method to look for
   */
  private def vcEqSym(vc: ClassDenotation, eqName: TermName)(implicit ctx: Context): Symbol = {
    vc.info.member(eqName)
      .suchThat(sym => sym.info.paramTypess match {
        case List(List(paramType)) =>
          paramType.isRef(vc.symbol) && sym.info.resultType.isRef(defn.BooleanClass)
        case _ =>
          false
      }).symbol
  }

  /** For a value class V, we generate the method:
   *    def `eqName`(x$0: V): Boolean
   *  where the parameter `eqName` must be either `nme.EQ` or `nme.NE`
   *
   *  In both cases, the generated method is guaranteed to behave like the
   *  method with the same name in Any:
   *    def `eqName`(x$0: Any): Boolean
   *
   *  Therefore, if `equals` is overriden in V or one of its supertrait, then we
   *  simply forward to it:
   *    def ==(x$0: V): Boolean = this.equals(x$0)
   *  or:
   *    def !=(x$0: V): Boolean = !this.equals(x$0)
   *  This is not ideal because the argument passed to `equals` will have to be boxed.
   *
   *  If `equals` is not overriden in V or one of its supertrait, then we know
   *  that it is synthesized in [[SyntheticMethods]], therefore we can write our method as:
   *    def ==(x$0: V): Boolean = this.underlying == x$0.underlying
   *  or:
   *    def !=(x$0: V): Boolean = this.underlying != x$0.underlying
   *  The corresponding extension method will be:
   *    def ==$extension($this: U, x$0: U): Boolean = $this == x$0
   *  or:
   *    def !=$extension($this: U, x$0: U): Boolean = $this != x$0
   *  which does not involve any boxing.
   */
  private def eqDefDef(vc: ClassDenotation, eqName: TermName)(implicit ctx: Context) = {
    assert((eqName eq nme.EQ) || (eqName eq nme.NE))

    val thisTree = This(vc.classSymbol)
    val eqSym = vcEqSym(vc, eqName).asTerm

    val hasCustomEquals = !defn.Any_equals.matchingMember(vc.thisType).is(Synthetic)

    if (hasCustomEquals) {
      // def ==(x$0: V): Boolean = this.equals(x$0)
      // or:
      // def !=(x$0: V): Boolean = !this.equals(x$0)
      DefDef(eqSym, { paramss =>
        val List(List(param)) = paramss
        val eqBody = thisTree.select(nme.equals_).appliedTo(param)
        if (eqName eq nme.NE)
          eqBody.select(defn.Boolean_!)
        else
          eqBody
      })
    } else {
      val underlying = valueClassUnbox(vc)
    val thisUnderlying = thisTree.select(underlying)
      // def ==(x$0: V): Boolean = this.underlying == x$0.underlying
      // or:
      // def !=(x$0: V): Boolean = this.underlying != x$0.underlying
      DefDef(eqSym, { paramss =>
        val List(List(param)) = paramss
        val thatUnderlying = param.select(underlying)
        applyOverloaded(thisUnderlying, eqName, List(thatUnderlying), Nil, defn.BooleanType)
      })
    }
  }

  /** For a value class V, we synthesize the following methods if they don't exist:
   *    def ==(x$0: V): Boolean
   *    def !=(x$0: V): Boolean
   */
  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = ctx.owner.asClass

    if (isDerivedValueClass(cls)) {
      val newDefs = new ListBuffer[Tree]
      eqNames foreach { eqName =>
        if (vcEqSym(cls, eqName).is(Synthetic)) {
          newDefs += eqDefDef(cls, eqName)
        }
      }
      cpy.Template(tree)(body = tree.body ++ newDefs)
    } else tree
  }

  /** For a value class V, let v1: V and v2: V, we replace:
   *    v1 == v2 (where `==` is `def ==(x$0: Any): Boolean`)
   *  by:
   *    v1 == v2 (where `==` is `def ==(x$0: V): Boolean`)
   *  if a symbol for that method exists.
   *
   *  We do the same for `!=`.
   */
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Any_== = defn.Any_==
    val Any_!= = defn.Any_!=
    tree match {
      case BinaryOp(arg1, op @ (Any_== | Any_!=), arg2) =>
        val tp1 = arg1.tpe.widenDealias
        val tp2 = arg2.tpe.widenDealias
        val sym1 = tp1.typeSymbol
        if (tp1 == tp2 && isDerivedValueClass(sym1)) {
          val eqSym = vcEqSym(sym1.asClass, op.name.asTermName)
          if (eqSym.exists)
            arg1.selectWithSig(eqSym).appliedTo(arg2)
          else tree
        } else tree
      case _ =>
        tree
    }
  }
}
