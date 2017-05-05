package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import TreeTransforms._, Phases.Phase
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import TypeErasure.ErasedValueType, ValueClasses._

/** This phase erases ErasedValueType to their underlying type.
 *  It also removes the synthetic cast methods u2evt$ and evt2u$ which are
 *  no longer needed afterwards.
 *  Finally, it checks that we don't introduce "double definitions" of pairs
 *  of methods that now have the same signature but were not considered matching
 *  before erasure.
 */
class ElimErasedValueType extends MiniPhaseTransform with InfoTransformer {

  import tpd._

  override def phaseName: String = "elimErasedValueType"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = sym match {
    case sym: ClassSymbol if sym is ModuleClass =>
      sym.companionClass match {
        case origClass: ClassSymbol if isDerivedValueClass(origClass) =>
          val cinfo = tp.asInstanceOf[ClassInfo]
          val decls1 = cinfo.decls.cloneScope
          ctx.atPhase(this.next) { implicit ctx =>
            // Remove synthetic cast methods introduced by ExtensionMethods,
            // they are no longer needed after this phase.
            decls1.unlink(cinfo.decl(nme.U2EVT).symbol)
            decls1.unlink(cinfo.decl(nme.EVT2U).symbol)
          }
          cinfo.derivedClassInfo(decls = decls1)
        case _ =>
          tp
      }
    case _ =>
      elimEVT(tp)
  }

  def elimEVT(tp: Type)(implicit ctx: Context): Type = tp match {
    case ErasedValueType(_, underlying) =>
      elimEVT(underlying)
    case tp: MethodType =>
      val paramTypes = tp.paramInfos.mapConserve(elimEVT)
      val retType = elimEVT(tp.resultType)
      tp.derivedLambdaType(tp.paramNames, paramTypes, retType)
    case _ =>
      tp
  }

  def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimEVT(tree.tpe))

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree

    // The casts to and from ErasedValueType are no longer needed once ErasedValueType
    // has been eliminated.
    val t =
      if (fun.symbol.isValueClassConvertMethod)
        args.head
      else
        tree
    transformTypeOfTree(t)
  }

  /** Check that we don't have pairs of methods that override each other after
   *  this phase, yet do not have matching types before erasure.
   *  The before erasure test is performed after phase elimRepeated, so we
   *  do not need to special case pairs of `T* / Seq[T]` parameters.
   */
  private def checkNoClashes(root: Symbol)(implicit ctx: Context) = {
    val opc = new OverridingPairs.Cursor(root) {
      override def exclude(sym: Symbol) =
        !sym.is(Method) || sym.is(Bridge) || super.exclude(sym)
      override def matches(sym1: Symbol, sym2: Symbol) =
        sym1.signature == sym2.signature
    }
    def checkNoConflict(sym1: Symbol, sym2: Symbol, info: Type)(implicit ctx: Context): Unit = {
      val site = root.thisType
      val info1 = site.memberInfo(sym1)
      val info2 = site.memberInfo(sym2)
      if (!info1.matchesLoosely(info2))
        ctx.error(
            em"""double definition:
                |$sym1: $info1 in ${sym1.owner} ${sym1.flags} and
                |$sym2: $info2 in ${sym2.owner} ${sym2.flags}
                |have same type after erasure: $info""",
            root.pos)
    }
    val earlyCtx = ctx.withPhase(ctx.elimRepeatedPhase.next)
    while (opc.hasNext) {
      val sym1 = opc.overriding
      val sym2 = opc.overridden
      checkNoConflict(sym1, sym2, sym1.info)(earlyCtx)
      opc.next()
    }
  }

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    checkNoClashes(tree.symbol)
    tree
  }

  override def transformInlined(tree: Inlined)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  // FIXME: transformIf and transformBlock won't be required anymore once #444 is fixed.
  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
}
