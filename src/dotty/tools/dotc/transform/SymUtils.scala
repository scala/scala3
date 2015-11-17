package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import SymDenotations._
import Decorators._
import Names._
import StdNames._
import NameOps._
import Flags._
import Annotations._
import language.implicitConversions

object SymUtils {
  implicit def decorateSymbol(sym: Symbol): SymUtils = new SymUtils(sym)
  implicit def decorateSymDenot(d: SymDenotation): SymUtils = new SymUtils(d.symbol)
}

/** A decorator that provides methods on symbols
 *  that are needed in the transformer pipeline.
 */
class SymUtils(val self: Symbol) extends AnyVal {
  import SymUtils._

  /** All traits implemented by a class or trait except for those inherited through the superclass. */
  def directlyInheritedTraits(implicit ctx: Context) = {
    val superCls = self.asClass.superClass
    val baseClasses = self.asClass.baseClasses
    if (baseClasses.isEmpty) Nil
    else baseClasses.tail.takeWhile(_ ne superCls).reverse
  }

  /** All traits implemented by a class, except for those inherited through the superclass.
   *  The empty list if `self` is a trait.
   */
  def mixins(implicit ctx: Context) = {
    if (self is Trait) Nil
    else directlyInheritedTraits
  }

  def isTypeTestOrCast(implicit ctx: Context): Boolean =
    self == defn.Any_asInstanceOf || self == defn.Any_isInstanceOf

  def isVolatile(implicit ctx: Context) = self.hasAnnotation(defn.VolatileAnnot)

  def isAnyOverride(implicit ctx: Context) = self.is(Override) || self.is(AbsOverride)
    // careful: AbsOverride is a term only flag. combining with Override would catch only terms.

  /** If this is a constructor, its owner: otherwise this. */
  final def skipConstructor(implicit ctx: Context): Symbol =
    if (self.isConstructor) self.owner else self

  /** The logically enclosing method or class for this symbol.
   *  Instead of constructors one always picks the enclosing class.
   */
  final def enclosure(implicit ctx: Context) = self.owner.enclosingMethod.skipConstructor

  /** Apply symbol/symbol substitution to this symbol */
  def subst(from: List[Symbol], to: List[Symbol]): Symbol = {
    def loop(from: List[Symbol], to: List[Symbol]): Symbol =
      if (from.isEmpty) self
      else if (self eq from.head) to.head
      else loop(from.tail, to.tail)
    loop(from, to)
  }

  def accessorNamed(name: TermName)(implicit ctx: Context): Symbol =
    self.owner.info.decl(name).suchThat(_ is Accessor).symbol

  def termParamAccessors(implicit ctx: Context): List[Symbol] =
    self.info.decls.filter(_ is TermParamAccessor).toList

  def caseAccessors(implicit ctx:Context) =
    self.info.decls.filter(_ is CaseAccessor).toList

  def getter(implicit ctx: Context): Symbol =
    if (self.isGetter) self else accessorNamed(self.asTerm.name.getterName)

  def setter(implicit ctx: Context): Symbol =
    if (self.isSetter) self
    else accessorNamed(self.asTerm.name.setterName)

  def field(implicit ctx: Context): Symbol =
    self.owner.info.decl(self.asTerm.name.fieldName).suchThat(!_.is(Method)).symbol

  def isField(implicit ctx: Context): Boolean =
    self.isTerm && !self.is(Method)

  def implClass(implicit ctx: Context): Symbol =
    self.owner.info.decl(self.name.implClassName).symbol

  def annotationsCarrying(meta: ClassSymbol)(implicit ctx: Context): List[Annotation] =
    self.annotations.filter(_.symbol.hasAnnotation(meta))

  def withAnnotationsCarrying(from: Symbol, meta: ClassSymbol)(implicit ctx: Context): self.type = {
    self.addAnnotations(from.annotationsCarrying(meta))
    self
  }

  def registerCompanionMethod(name: Name, target: Symbol)(implicit ctx: Context) = {
    if (!self.unforcedDecls.lookup(name).exists) {
      val companionMethod = ctx.synthesizeCompanionMethod(name, target, self)
      if (companionMethod.exists) {
        companionMethod.entered
      }
    }
  }
}
