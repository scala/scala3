package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import Annotations._
import StdNames._
import NameOps._
import NameKinds.{ExpandedName, TraitSetterName}
import ast.Trees._

/** This phase augments Scala2 traits with implementation classes and with additional members
 *  needed for mixin composition.
 *  These symbols would have been added between Unpickling and Mixin in the Scala2 pipeline.
 *  Specifcally, it adds
 *
 *   - an implementation class which defines a trait constructor and trait method implementations
 *   - trait setters for vals defined in traits
 *
 *  Furthermore, it expands the names of all private getters and setters as well as super accessors in the trait and makes
 *  them not-private.
 */
class AugmentScala2Traits extends MiniPhase with IdentityDenotTransformer with FullParameterization { thisPhase =>
  import ast.tpd._

  override def changesMembers = true

  override def phaseName: String = "augmentScala2Traits"

  override def rewiredTarget(referenced: Symbol, derived: Symbol)(implicit ctx: Context) = NoSymbol

  override def transformTemplate(impl: Template)(implicit ctx: Context) = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins)
      if (mixin.is(Scala2x))
        augmentScala2Trait(mixin, cls)
    impl
  }

  private def augmentScala2Trait(mixin: ClassSymbol, cls: ClassSymbol)(implicit ctx: Context): Unit = {
    if (mixin.implClass.is(Scala2x)) () // nothing to do, mixin was already augmented
    else {
      //println(i"creating new implclass for $mixin ${mixin.implClass}")
      val ops = new MixinOps(cls, thisPhase)
      import ops._

      val implClass = ctx.newCompleteClassSymbol(
        owner = mixin.owner,
        name = mixin.name.implClassName,
        flags = Abstract | Scala2x | ImplClass,
        parents = defn.ObjectType :: Nil,
        assocFile = mixin.assocFile).enteredAfter(thisPhase)

      def implMethod(meth: TermSymbol): Symbol = {
        val mold =
          if (meth.isConstructor)
            meth.copySymDenotation(
              name = nme.TRAIT_CONSTRUCTOR,
              info = MethodType(Nil, defn.UnitType))
          else meth.ensureNotPrivate
        meth.copy(
          owner = implClass,
          name = mold.name.asTermName,
          flags = Method | JavaStatic,
          info = fullyParameterizedType(mold.info, mixin))
      }

      def traitSetter(getter: TermSymbol) =
        getter.copy(
          name = getter.ensureNotPrivate.name
                  .expandedName(getter.owner, TraitSetterName)
                  .asTermName.setterName,
          flags = Method | Accessor,
          info = MethodType(getter.info.resultType :: Nil, defn.UnitType))

      for (sym <- mixin.info.decls) {
        if (needsForwarder(sym) || sym.isConstructor || sym.isGetter && sym.is(Lazy) || sym.is(Method, butNot = Deferred))
          implClass.enter(implMethod(sym.asTerm))
        if (sym.isGetter)
          if (sym.is(Lazy)) {
            if (!sym.hasAnnotation(defn.VolatileAnnot))
              sym.addAnnotation(Annotation(defn.VolatileAnnot, Nil))
          }
          else if (!sym.is(Deferred) && !sym.setter.exists &&
                   !sym.info.resultType.isInstanceOf[ConstantType])
            traitSetter(sym.asTerm).enteredAfter(thisPhase)
        if ((sym.isBoth(Private, and = Accessor) && !sym.name.is(ExpandedName) &&
          (sym.isGetter || sym.isSetter)) // strangely, Scala 2 fields are also methods that have Accessor set.
          || sym.isSuperAccessor) // scala2 superaccessors are pickled as private, but are compiled as public expanded
          sym.ensureNotPrivate.installAfter(thisPhase)
      }
      ctx.log(i"Scala2x trait decls of $mixin = ${mixin.info.decls.toList.map(_.showDcl)}%\n %")
      ctx.log(i"Scala2x impl decls of $mixin = ${implClass.info.decls.toList.map(_.showDcl)}%\n %")
    }
  }
}
