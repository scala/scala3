package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import SymDenotations._
import Names._
import NameOps._
import StdNames._
import NameKinds._
import Flags._
import Annotations._
import ValueClasses.isDerivedValueClass
import Decorators._

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
  def directlyInheritedTraits(implicit ctx: Context): List[ClassSymbol] = {
    val superCls = self.asClass.superClass
    val baseClasses = self.asClass.baseClasses
    if (baseClasses.isEmpty) Nil
    else baseClasses.tail.takeWhile(_ ne superCls).reverse
  }

  /** All traits implemented by a class, except for those inherited through the superclass.
   *  The empty list if `self` is a trait.
   */
  def mixins(implicit ctx: Context): List[ClassSymbol] = {
    if (self is Trait) Nil
    else directlyInheritedTraits
  }

  def isTypeTest(implicit ctx: Context): Boolean =
    self == defn.Any_isInstanceOf || self == defn.Any_typeTest

  def isTypeCast(implicit ctx: Context): Boolean =
    self == defn.Any_asInstanceOf || self == defn.Any_typeCast

  def isTypeTestOrCast(implicit ctx: Context): Boolean =
    isTypeCast || isTypeTest

  def isThreadUnsafe(implicit ctx: Context): Boolean = self.hasAnnotation(defn.ThreadUnsafeAnnot)

  def isVolatile(implicit ctx: Context): Boolean = self.hasAnnotation(defn.VolatileAnnot)

  def isAnyOverride(implicit ctx: Context): Boolean = self.is(Override) || self.is(AbsOverride)
    // careful: AbsOverride is a term only flag. combining with Override would catch only terms.

  def isSuperAccessor(implicit ctx: Context): Boolean = self.name.is(SuperAccessorName)

  /** Is this a type or term parameter or a term parameter accessor? */
  def isParamOrAccessor(implicit ctx: Context): Boolean =
    self.is(Param) || self.is(ParamAccessor)

  def derivesFromJavaEnum(implicit ctx: Context) =
    self.is(Enum, butNot = Case) &&
    self.info.parents.exists(p => p.typeSymbol == defn.JavaEnumClass)

  /** Is this a case class for which a product mirror is generated?
   *  Excluded are value classes, abstract classes and case classes with more than one
   *  parameter section.
   */
  def whyNotGenericProduct(implicit ctx: Context): String =
    if (!self.is(CaseClass)) "it is not a case class"
    else if (self.is(Abstract)) "it is an abstract class"
    else if (self.primaryConstructor.info.paramInfoss.length != 1) "it takes more than one parameter list"
    else if (isDerivedValueClass(self)) "it is a value class"
    else ""

  def isGenericProduct(implicit ctx: Context): Boolean = whyNotGenericProduct.isEmpty

  /** Is this a sealed class or trait for which a sum mirror is generated?
   *  It must satisfy the following conditions:
   *   - it has at least one child class or object
   *   - none of its children are anonymous classes
   *   - all of its children are addressable through a path from its companion object
   *   - all of its children are generic products or singletons
   */
  def whyNotGenericSum(implicit ctx: Context): String =
    if (!self.is(Sealed))
      s"it is not a sealed ${if (self.is(Trait)) "trait" else "class"}"
    else {
      val children = self.children
      val companion = self.linkedClass
      def problem(child: Symbol) = {

        def isAccessible(sym: Symbol): Boolean =
          companion.isContainedIn(sym) || sym.is(Module) && isAccessible(sym.owner)

        if (child == self) "it has anonymous or inaccessible subclasses"
        else if (!isAccessible(child.owner)) i"its child $child is not accessible"
        else if (!child.isClass) ""
        else {
          val s = child.whyNotGenericProduct
          if (s.isEmpty) s
          else i"its child $child is not a generic product because $s"
        }
      }
      if (children.isEmpty) "it does not have subclasses"
      else children.map(problem).find(!_.isEmpty).getOrElse("")
    }

  def isGenericSum(implicit ctx: Context): Boolean = whyNotGenericSum.isEmpty

  /** If this is a constructor, its owner: otherwise this. */
  final def skipConstructor(implicit ctx: Context): Symbol =
    if (self.isConstructor) self.owner else self

  /** The closest properly enclosing method or class of this symbol. */
  final def enclosure(implicit ctx: Context): Symbol = {
    self.owner.enclosingMethodOrClass
  }

  /** The closest enclosing method or class of this symbol */
  @tailrec final def enclosingMethodOrClass(implicit ctx: Context): Symbol =
    if (self.is(Method) || self.isClass) self
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

  def accessorNamed(name: TermName)(implicit ctx: Context): Symbol =
    self.owner.info.decl(name).suchThat(_ is Accessor).symbol

  def caseAccessors(implicit ctx: Context): List[Symbol] =
    self.info.decls.filter(_ is CaseAccessor)

  def getter(implicit ctx: Context): Symbol =
    if (self.isGetter) self else accessorNamed(self.asTerm.name.getterName)

  def setter(implicit ctx: Context): Symbol =
    if (self.isSetter) self
    else accessorNamed(self.asTerm.name.setterName)

  def field(implicit ctx: Context): Symbol = {
    val thisName = self.name.asTermName
    val fieldName =
      if (self.hasAnnotation(defn.ScalaStaticAnnot)) thisName.getterName
      else thisName.fieldName
    self.owner.info.decl(fieldName).suchThat(!_.is(Method)).symbol
  }

  def isField(implicit ctx: Context): Boolean =
    self.isTerm && !self.is(Method)

  def annotationsCarrying(meta: ClassSymbol)(implicit ctx: Context): List[Annotation] =
    self.annotations.filter(_.symbol.hasAnnotation(meta))

  def withAnnotationsCarrying(from: Symbol, meta: ClassSymbol)(implicit ctx: Context): self.type = {
    self.addAnnotations(from.annotationsCarrying(meta))
    self
  }

  /** Does this symbol refer to anonymous classes synthesized by enum desugaring? */
  def isEnumAnonymClass(implicit ctx: Context): Boolean =
    self.isAnonymousClass && (self.owner.name.eq(nme.DOLLAR_NEW) || self.owner.is(CaseVal))

  /** Is this symbol defined locally (i.e. at some level owned by a term) and
   *  defined in a different toplevel class than its supposed parent class `cls`?
   *  Such children are not pickled, and have to be reconstituted manually.
   */
  def isInaccessibleChildOf(cls: Symbol)(implicit ctx: Context): Boolean =
    self.isLocal && !cls.topLevelClass.isLinkedWith(self.topLevelClass)

  /** If this is a sealed class, its known children in the order of textual occurrence */
  def children(implicit ctx: Context): List[Symbol] = {
    if (self.isType)
      self.setFlag(ChildrenQueried)
    self.annotations.collect {
      case Annotation.Child(child) => child
    }.reverse
  }

  def hasAnonymousChild(implicit ctx: Context): Boolean =
    children.exists(_ `eq` self)

  /** Is symbol directly or indirectly owned by a term symbol? */
  @tailrec final def isLocal(implicit ctx: Context): Boolean = {
    val owner = self.maybeOwner
    if (!owner.exists) false
    else if (owner.isTerm) true
    else if (owner.is(Package)) false
    else owner.isLocal
  }

  /** The typeRef with wildcard arguments for each type parameter */
  def rawTypeRef(implicit ctx: Context) =
    self.typeRef.appliedTo(self.typeParams.map(_ => TypeBounds.empty))

  /** Is symbol a quote operation? */
  def isQuote(implicit ctx: Context): Boolean =
    self == defn.InternalQuoted_exprQuote || self == defn.InternalQuoted_typeQuote

  /** Is symbol a splice operation? */
  def isSplice(implicit ctx: Context): Boolean =
    self == defn.InternalQuoted_exprSplice || self == defn.QuotedType_splice
}
