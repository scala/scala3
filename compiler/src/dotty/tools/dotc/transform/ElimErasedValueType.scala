package dotty.tools
package dotc
package transform

import ast.{Trees, tpd}
import core.*, core.Decorators.*
import MegaPhase.*
import Types.*, Contexts.*, Flags.*, DenotTransformers.*, Phases.*
import Symbols.*, StdNames.*, Trees.*
import TypeErasure.ErasedValueType, ValueClasses.*
import reporting.*
import NameKinds.SuperAccessorName

object ElimErasedValueType {
  val name: String = "elimErasedValueType"
  val description: String = "expand erased value types to their underlying implementation types"

  def elimEVT(tp: Type)(using Context): Type = tp match {
    case ErasedValueType(_, underlying) =>
      elimEVT(underlying)
    case tp: MethodType =>
      val paramTypes = tp.paramInfos.mapConserve(elimEVT)
      val retType = elimEVT(tp.resultType)
      tp.derivedLambdaType(tp.paramNames, paramTypes, retType)
    case _ =>
      tp
  }
}

/** This phase erases ErasedValueType to their underlying type.
 *  It also removes the synthetic cast methods u2evt$ and evt2u$ which are
 *  no longer needed afterwards.
 *  Finally, it checks that we don't introduce "double definitions" of pairs
 *  of methods that now have the same signature but were not considered matching
 *  before erasure.
 */
class ElimErasedValueType extends MiniPhase with InfoTransformer { thisPhase =>

  import tpd.*
  import ElimErasedValueType.elimEVT

  override def phaseName: String = ElimErasedValueType.name

  override def description: String = ElimErasedValueType.description

  override def runsAfter: Set[String] = Set(Erasure.name)

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type = sym match {
    case sym: ClassSymbol if sym.is(ModuleClass) =>
      sym.companionClass match {
        case origClass: ClassSymbol if isDerivedValueClass(origClass) =>
          val cinfo = tp.asInstanceOf[ClassInfo]
          val decls1 = cinfo.decls.cloneScope
          // Remove synthetic cast methods introduced by ExtensionMethods,
          // they are no longer needed after this phase.
          decls1.unlink(cinfo.decl(nme.U2EVT).symbol)
          decls1.unlink(cinfo.decl(nme.EVT2U).symbol)
          cinfo.derivedClassInfo(decls = decls1)
        case _ =>
          tp
      }
    case _ =>
      elimEVT(tp)
  }

  def transformTypeOfTree(tree: Tree)(using Context): Tree =
    tree.withType(elimEVT(tree.tpe))

  override def transformApply(tree: Apply)(using Context): Tree = {
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
   */
  private def checkNoClashes(root: Symbol)(using Context) = {
    val opc = atPhase(thisPhase) {
      new OverridingPairs.Cursor(root) {
        override def isSubParent(parent: Symbol, bc: Symbol)(using Context) =
          // Need to compute suparents before erasure to not filter out parents
          // that are bypassed with different types. See neg/11719a.scala.
          atPhase(elimRepeatedPhase.next) { super.isSubParent(parent, bc) }
        override def exclude(sym: Symbol) =
          !sym.is(Method) || sym.is(Bridge) || super.exclude(sym)
        override def matches(sym1: Symbol, sym2: Symbol) =
          sym1.signature == sym2.signature
      }
    }

    def checkNoConflict(sym1: Symbol, sym2: Symbol, info: Type)(using Context): Unit = {
      val site = root.thisType
      val info1 = site.memberInfo(sym1)
      val info2 = site.memberInfo(sym2)
      // PolyFunction apply methods will be eliminated later during
      // ElimPolyFunction, so we let them pass here.
      def bothPolyApply =
        sym1.name == nme.apply &&
        (sym1.owner.derivesFrom(defn.PolyFunctionClass) ||
         sym2.owner.derivesFrom(defn.PolyFunctionClass))

      // super-accessors start as private, and their expanded name can clash after
      // erasure. TODO: Verify that this is OK.
      def bothSuperAccessors = sym1.name.is(SuperAccessorName) && sym2.name.is(SuperAccessorName)
      if (sym1.name != sym2.name && !bothSuperAccessors ||
          !info1.matchesLoosely(info2) && !bothPolyApply)
        report.error(DoubleDefinition(sym1, sym2, root), root.srcPos)
    }
    while (opc.hasNext) {
      val sym1 = opc.overriding
      val sym2 = opc.overridden
      // Do the test at the earliest phase where both symbols existed.
      val phaseId =
        sym1.originDenotation.validFor.firstPhaseId max sym2.originDenotation.validFor.firstPhaseId
      atPhase(elimByNamePhase.next)(checkNoConflict(sym1, sym2, sym1.info))
      opc.next()
    }
  }

  override def prepareForTypeDef(tree: TypeDef)(using Context): Context = {
    checkNoClashes(tree.symbol)
    ctx
  }

  override def transformInlined(tree: Inlined)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformIdent(tree: Ident)(using Context): Tree =
    transformTypeOfTree(tree)
  override def transformSelect(tree: Select)(using Context): Tree =
    transformTypeOfTree(tree)
  override def transformTypeTree(tree: TypeTree)(using Context): Tree =
    transformTypeOfTree(tree)
}
