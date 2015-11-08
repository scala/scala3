package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import util.Positions._
import Names._
import collection.mutable

/** This phase replaces calls to DottyPredef.classTag by code that synthesizes appropriate ClassTag
  */
class ClassTags extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  private var classTagCache: Symbol = null
  private var typeTagCache: Symbol  = null
  private var scala2ClassTagModule: Symbol = null


  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    classTagCache = defn.DottyPredefModule.requiredMethod(nme.classTag)
    typeTagCache = defn.DottyPredefModule.requiredMethod(nme.typeTag)
    scala2ClassTagModule = ctx.requiredModule("scala.reflect.ClassTag")
    this
  }

  override def phaseName: String = "classTags"

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    if (tree.fun.symbol eq classTagCache) {
      val tp = tree.args.head.tpe
      val defn = ctx.definitions
      val (elemType, ndims) = tp match {
        case defn.MultiArrayOf(elem, ndims) => (elem, ndims)
        case _ => (tp, 0)
      }

      val claz = tp.classSymbol
      val elemClaz = elemType.classSymbol
      assert(!claz.isPrimitiveValueClass) // should be inserted by typer
      val elemTag = 
        if (elemClaz.isPrimitiveValueClass || elemClaz == defn.NothingClass || elemClaz == defn.NullClass)
          ref(defn.DottyPredefModule).select(s"${elemClaz.name}ClassTag".toTermName)
        else if (ValueClasses.isDerivedValueClass(elemClaz)) 
          ref(claz.companionModule)
        else if (elemClaz eq defn.AnyClass) 
          ref(scala2ClassTagModule).select(nme.Any)
        else {
          val erazedTp = TypeErasure.erasure(elemType).classSymbol.typeRef
          ref(scala2ClassTagModule).select(nme.apply)
            .appliedToType(erazedTp).appliedTo(Literal(Constant(erazedTp)))
        }
      (1 to ndims).foldLeft(elemTag)((arr, level) => Select(arr, nme.wrap).ensureApplied).ensureConforms(tree.tpe)
    } else tree
}
