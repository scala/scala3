package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import Types._
import Decorators._
import DenotTransformers._
import Annotations._
import StdNames._
import NameOps._
import NameKinds.{ExpandedName, TraitSetterName}
import ast.Trees._

object AugmentScala2Traits {
  val name: String = "augmentScala2Traits"
}

/** This phase augments Scala2 traits with additional members needed for mixin composition.
 *
 *  These symbols would have been added between Unpickling and Mixin in the Scala2 pipeline.
 *
 *  Specifically, we:
 *   - Add trait setters for vals defined in traits.
 *   - Expand the names of all private getters and setters as well as super accessors in the trait and make
 *     not-private.
 */
class AugmentScala2Traits extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def changesMembers: Boolean = true

  override def phaseName: String = AugmentScala2Traits.name

  override def transformTemplate(impl: Template)(implicit ctx: Context): Template = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins) {
      val erasedMixin = TypeErasure.normalizeClass(mixin)
      if (erasedMixin.is(Scala2x) && !erasedMixin.is(Scala2xPartiallyAugmented))
        augmentScala2Trait(erasedMixin)
    }
    impl
  }

  private def augmentScala2Trait(mixin: ClassSymbol)(implicit ctx: Context): Unit = {
    def traitSetter(getter: TermSymbol) =
      getter.copy(
        name = getter.ensureNotPrivate.name
          .expandedName(getter.owner, TraitSetterName)
          .asTermName.setterName,
        flags = Method | Accessor,
        info = MethodType(getter.info.resultType :: Nil, defn.UnitType))

    for (sym <- mixin.info.decls) {
      if (sym.isGetter && !sym.is(LazyOrDeferred) && !sym.setter.exists &&
          !sym.info.resultType.isInstanceOf[ConstantType])
        traitSetter(sym.asTerm).enteredAfter(thisPhase)
      if ((sym.is(PrivateAccessor) && !sym.name.is(ExpandedName) &&
        (sym.isGetter || sym.isSetter)) // strangely, Scala 2 fields are also methods that have Accessor set.
        || sym.isSuperAccessor) // scala2 superaccessors are pickled as private, but are compiled as public expanded
        sym.ensureNotPrivate.installAfter(thisPhase)
    }
    mixin.setFlag(Scala2xPartiallyAugmented)
  }
}
