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

  def isTypeTestOrCast(implicit ctx: Context): Boolean =
    self == defn.Any_asInstanceOf || self == defn.Any_isInstanceOf

  def isVolatile(implicit ctx: Context) = self.hasAnnotation(defn.VolatileAnnot)

  /** If this is a constructor, its owner: otherwise this. */
  final def skipConstructor(implicit ctx: Context): Symbol =
    if (self.isConstructor) self.owner else self

  final def isAnonymousFunction(implicit ctx: Context): Boolean =
    self.is(Method) && (self.denot.initial.asSymDenotation.name startsWith nme.ANON_FUN)

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

  def caseAccessors(implicit ctx:Context) =
    self.decls.filter(_ is CaseAccessor).toList

  def getter(implicit ctx: Context): Symbol =
    if (self.isGetter) self else accessorNamed(self.asTerm.name.getterName)

  def setter(implicit ctx: Context): Symbol =
    if (self.isSetter) self
    else accessorNamed(self.asTerm.name.setterName) orElse
         accessorNamed(self.asTerm.name.traitSetterName)

  def field(implicit ctx: Context): Symbol =
    self.owner.info.decl(self.asTerm.name.fieldName).suchThat(!_.is(Method)).symbol

  /** `fullName` where `$' is the separator character */
  def flatName(implicit ctx: Context): Name = self.fullNameSeparated('$')

  /** The traits mixed into this class in linearization order.
   *  These are all inherited traits that are not also inherited by the superclass
   */
  def mixins(implicit ctx: Context): List[ClassSymbol] = {
    val cls = self.asClass
    val superCls = cls.classInfo.parents.head.symbol
    cls.baseClasses.tail.takeWhile(_ ne superCls)
  }

  def initializer(implicit ctx: Context): TermSymbol =
    self.owner.info.decl(InitializerName(self.asTerm.name)).symbol.asTerm

  def isField(implicit ctx: Context): Boolean =
    self.isTerm && !self.is(Method)
}
