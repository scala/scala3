package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
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

/** This phase augments Scala2 traits to fix up super accessors.
 *
 *  Strangely, Scala 2 super accessors are pickled as private, but are compiled as public expanded.
 *  In this phase we expand them and make them non-private, so that `ResolveSuper` does something meaningful.
 *
 *  TODO Should we merge this into `ResolveSuper` at this point?
 */
class AugmentScala2Traits extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def changesMembers: Boolean = true

  override def phaseName: String = AugmentScala2Traits.name

  override def transformTemplate(impl: Template)(using Context): Template = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins) {
      val erasedMixin = TypeErasure.normalizeClass(mixin)
      if (erasedMixin.is(Scala2x) && !erasedMixin.is(Scala2xPartiallyAugmented))
        augmentScala2Trait(erasedMixin)
    }
    impl
  }

  private def augmentScala2Trait(mixin: ClassSymbol)(using Context): Unit = {
    for (sym <- mixin.info.decls) {
      if (sym.isSuperAccessor)
        sym.ensureNotPrivate.installAfter(thisPhase)
    }
    mixin.setFlagFrom(thisPhase, Scala2xPartiallyAugmented)
  }
}
