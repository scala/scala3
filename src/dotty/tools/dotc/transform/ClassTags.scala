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
    val predefClass = defn.DottyPredefModule.moduleClass.asClass
    classTagCache = ctx.requiredMethod(predefClass,  names.classTag)
    typeTagCache = ctx.requiredMethod(predefClass,  names.typeTag)
    scala2ClassTagModule = ctx.requiredModule("scala.reflect.ClassTag")
    this
  }

  override def phaseName: String = "classTags"

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    if (tree.fun.symbol eq classTagCache) {
      val tp = tree.args.head.tpe
      val claz = tp.classSymbol
      assert(!claz.isPrimitiveValueClass) // should be inserted by typer
      if (ValueClasses.isDerivedValueClass(claz)) ref(claz.companionModule)
      else if (claz eq defn.AnyClass) ref(scala2ClassTagModule).select(nme.Any).ensureConforms(tree.tpe)
      else ref(scala2ClassTagModule).select(nme.apply).appliedToType(tp).appliedTo(Literal(Constant(claz.typeRef)))
    } else tree
}
