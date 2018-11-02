package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import SymDenotations._
import Names._
import NameOps._
import NameKinds._
import Flags._
import Annotations._

import language.implicitConversions
import scala.annotation.tailrec

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
  def directlyInheritedTraits(implicit ctx: ContextRenamed): List[ClassSymbol] = {
    val superCls = self.asClass.superClass
    val baseClasses = self.asClass.baseClasses
    if (baseClasses.isEmpty) Nil
    else baseClasses.tail.takeWhile(_ ne superCls).reverse
  }

  /** All traits implemented by a class, except for those inherited through the superclass.
   *  The empty list if `self` is a trait.
   */
  def mixins(implicit ctx: ContextRenamed): List[ClassSymbol] = {
    if (self is Trait) Nil
    else directlyInheritedTraits
  }

  def isTypeTest(implicit ctx: ContextRenamed): Boolean =
    self == defn.Any_isInstanceOf || self == defn.Any_typeTest

  def isTypeTestOrCast(implicit ctx: ContextRenamed): Boolean =
    self == defn.Any_asInstanceOf || isTypeTest

  def isVolatile(implicit ctx: ContextRenamed): Boolean = self.hasAnnotation(defn.VolatileAnnot)

  def isAnyOverride(implicit ctx: ContextRenamed): Boolean = self.is(Override) || self.is(AbsOverride)
    // careful: AbsOverride is a term only flag. combining with Override would catch only terms.

  def isSuperAccessor(implicit ctx: ContextRenamed): Boolean = self.name.is(SuperAccessorName)

  /** A type or term parameter or a term parameter accessor */
  def isParamOrAccessor(implicit ctx: ContextRenamed): Boolean =
    self.is(Param) || self.is(ParamAccessor)

  /** If this is a constructor, its owner: otherwise this. */
  final def skipConstructor(implicit ctx: ContextRenamed): Symbol =
    if (self.isConstructor) self.owner else self

  /** The closest properly enclosing method or class of this symbol. */
  final def enclosure(implicit ctx: ContextRenamed): Symbol = {
    self.owner.enclosingMethodOrClass
  }

  /** The closest enclosing method or class of this symbol */
  @tailrec final def enclosingMethodOrClass(implicit ctx: ContextRenamed): Symbol =
    if (self.is(Method, butNot = Label) || self.isClass) self
    else if (self.exists) self.owner.enclosingMethodOrClass
    else NoSymbol

  /** Apply symbol/symbol substitution to this symbol */
  def subst(from: List[Symbol], to: List[Symbol]): Symbol = {
    @tailrec def loop(from: List[Symbol], to: List[Symbol]): Symbol =
      if (from.isEmpty) self
      else if (self eq from.head) to.head
      else loop(from.tail, to.tail)
    loop(from, to)
  }

  def accessorNamed(name: TermName)(implicit ctx: ContextRenamed): Symbol =
    self.owner.info.decl(name).suchThat(_ is Accessor).symbol

  def caseAccessors(implicit ctx: ContextRenamed): List[Symbol] =
    self.info.decls.filter(_ is CaseAccessor)

  def getter(implicit ctx: ContextRenamed): Symbol =
    if (self.isGetter) self else accessorNamed(self.asTerm.name.getterName)

  def setter(implicit ctx: ContextRenamed): Symbol =
    if (self.isSetter) self
    else accessorNamed(self.asTerm.name.setterName)

  def field(implicit ctx: ContextRenamed): Symbol = {
    val thisName = self.name.asTermName
    val fieldName =
      if (self.hasAnnotation(defn.ScalaStaticAnnot)) thisName.getterName
      else thisName.fieldName
    self.owner.info.decl(fieldName).suchThat(!_.is(Method)).symbol
  }

  def isField(implicit ctx: ContextRenamed): Boolean =
    self.isTerm && !self.is(Method)

  def implClass(implicit ctx: ContextRenamed): Symbol =
    self.owner.info.decl(self.name.implClassName).symbol

  def traitOfImplClass(implicit ctx: ContextRenamed): Symbol =
    self.owner.info.decl(self.name.traitOfImplClassName).symbol

  def annotationsCarrying(meta: ClassSymbol)(implicit ctx: ContextRenamed): List[Annotation] =
    self.annotations.filter(_.symbol.hasAnnotation(meta))

  def withAnnotationsCarrying(from: Symbol, meta: ClassSymbol)(implicit ctx: ContextRenamed): self.type = {
    self.addAnnotations(from.annotationsCarrying(meta))
    self
  }

  def registerCompanionMethod(name: Name, target: Symbol)(implicit ctx: ContextRenamed): Any = {
    if (!self.unforcedDecls.lookup(name).exists) {
      val companionMethod = ctx.synthesizeCompanionMethod(name, target, self)
      if (companionMethod.exists) {
        companionMethod.entered
      }
    }
  }

  /** If this symbol is an enum value or a named class, register it as a child
   *  in all direct parent classes which are sealed.
   *   @param  @late  If true, register only inaccessible children (all others are already
   *                  entered at this point).
   */
  def registerIfChild(late: Boolean = false)(implicit ctx: ContextRenamed): Unit = {
    def register(child: Symbol, parent: Type) = {
      val cls = parent.classSymbol
      if (cls.is(Sealed) && (!late || child.isInaccessibleChildOf(cls)))
        cls.addAnnotation(Annotation.Child(child))
    }
    if (self.isClass && !self.isAnonymousClass)
      self.asClass.classParents.foreach { parent =>
        val child = if (self.is(Module)) self.sourceModule else self
        register(child, parent)
      }
    else if (self.is(CaseVal, butNot = Method | Module))
      register(self, self.info)
  }

  /** Is this symbol defined locally (i.e. at some level owned by a term) and
   *  defined in a different toplevel class than its supposed parent class `cls`?
   *  Such children are not pickled, and have to be reconstituted manually.
   */
  def isInaccessibleChildOf(cls: Symbol)(implicit ctx: ContextRenamed): Boolean =
    self.isLocal && !cls.topLevelClass.isLinkedWith(self.topLevelClass)

  /** If this is a sealed class, its known children */
  def children(implicit ctx: ContextRenamed): List[Symbol] =
    self.annotations.collect {
      case Annotation.Child(child) => child
    }

  /** Is symbol directly or indirectly owned by a term symbol? */
  @tailrec final def isLocal(implicit ctx: ContextRenamed): Boolean = {
    val owner = self.owner
    if (owner.isTerm) true
    else if (owner.is(Package)) false
    else owner.isLocal
  }

  /** Is symbol a quote operation? */
  def isQuote(implicit ctx: ContextRenamed): Boolean =
    self == defn.QuotedExpr_apply || self == defn.QuotedType_apply

  /** Is symbol a splice operation? */
  def isSplice(implicit ctx: ContextRenamed): Boolean =
    self == defn.QuotedExpr_~ || self == defn.QuotedType_~
}
