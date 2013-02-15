package dotty.tools
package dotc
package core

import Periods._
import Transformers._
import Names._, Scopes._
import Flags._
import java.lang.AssertionError
import Decorators._
import Symbols._
import Contexts._
import SymDenotations._
import Types._, Annotations._, Positions._
import Denotations.{ Denotation, SingleDenotation, MultiDenotation }
import collection.mutable
import io.AbstractFile

/** Creation methods for symbols */
trait Symbols { this: Context =>

  def newLazySymbol[N <: Name](owner: Symbol, name: N, initFlags: FlagSet, completer: SymCompleter, off: Offset = NoOffset) =
    new Symbol(off, new LazySymDenotation(_, owner, name, initFlags, completer)) {
      type ThisName = N
    }

  def newLazyClassSymbol(owner: Symbol, name: TypeName, initFlags: FlagSet, completer: ClassCompleter, assocFile: AbstractFile = null, off: Offset = NoOffset) =
    new ClassSymbol(off, new LazyClassDenotation(_, owner, name, initFlags, completer, assocFile)(this))

  def newLazyModuleSymbols(owner: Symbol,
      name: TermName,
      flags: FlagSet,
      completer: ClassCompleter,
      assocFile: AbstractFile = null,
      off: Offset = NoOffset): (TermSymbol, ClassSymbol)
  = {
    val module = newLazySymbol(
      owner, name, flags | ModuleCreationFlags, new ModuleCompleter(condensed), off)
    val modcls = newLazyClassSymbol(
      owner, name.toTypeName, flags | ModuleClassCreationFlags, completer, assocFile, off)
    module.denot.asInstanceOf[LazySymDenotation].info =
      TypeRef(owner.thisType(ctx), modcls)
    modcls.denot.asInstanceOf[LazyClassDenotation].selfType =
      TermRef(owner.thisType, module)
    (module, modcls)
  }

  def newLazyPackageSymbols(owner: Symbol, name: TermName, completer: ClassCompleter) =
    newLazyModuleSymbols(owner, name, PackageCreationFlags, completer)

  def newSymbol[N <: Name](owner: Symbol, name: N, flags: FlagSet, info: Type, privateWithin: Symbol = NoSymbol, off: Offset = NoOffset) =
    new Symbol(off, CompleteSymDenotation(_, owner, name, flags, info, privateWithin)) {
      type ThisName = N
    }

  def newClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      privateWithin: Symbol = NoSymbol,
      optSelfType: Type = NoType,
      decls: Scope = newScope,
      assocFile: AbstractFile = null,
      off: Offset = NoOffset)
  =
    new ClassSymbol(off, new CompleteClassDenotation(
      _, owner, name, flags, parents, privateWithin, optSelfType, decls, assocFile)(this))

  def newModuleSymbols(
      owner: Symbol,
      name: TermName,
      flags: FlagSet,
      classFlags: FlagSet,
      parents: List[TypeRef],
      privateWithin: Symbol = NoSymbol,
      decls: Scope = newScope,
      assocFile: AbstractFile = null,
      off: Offset = NoOffset): (TermSymbol, ClassSymbol)
  = {
    val module = newLazySymbol(
      owner, name, flags | ModuleCreationFlags,
      new ModuleCompleter(condensed), off)
    val modcls = newClassSymbol(
      owner, name.toTypeName, classFlags | ModuleClassCreationFlags, parents, privateWithin,
      optSelfType = TermRef(owner.thisType, module),
      decls, assocFile, off)
    module.denot.asInstanceOf[LazySymDenotation].info =
      TypeRef(owner.thisType, modcls)
    (module, modcls)
  }

  def newPackageSymbols(
      owner: Symbol,
      name: TermName,
      decls: Scope = newScope) =
   newModuleSymbols(
     owner, name, PackageCreationFlags, PackageCreationFlags, Nil, NoSymbol, decls)

  def newStubSymbol(owner: Symbol, name: Name, file: AbstractFile = null): Symbol = {
    def stub = new StubCompleter(ctx.condensed)
    name match {
      case name: TermName => ctx.newLazyModuleSymbols(owner, name, EmptyFlags, stub, file)._1
      case name: TypeName => ctx.newLazyClassSymbol(owner, name, EmptyFlags, stub, file)
    }
  }

  def requiredPackage(path: PreName): TermSymbol =
    base.staticRef(path.toTermName).requiredSymbol(_.isPackage).asTerm

  def requiredClass(path: PreName): ClassSymbol =
    base.staticRef(path.toTypeName).requiredSymbol(_.isClass).asClass

  def requiredModule(path: PreName): TermSymbol =
    base.staticRef(path.toTermName).requiredSymbol(_.isModule).asTerm
}

object Symbols {

  /** A Symbol represents a Scala definition/declaration or a package.
   */
  class Symbol(val offset: Offset, denotf: Symbol => SymDenotation) extends DotClass {

    type ThisName <: Name

    /** Is symbol different from NoSymbol? */
    def exists = true

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol) = if (exists) this else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    private[this] var _id: Int = _

    /** The unique id of this symbol */
    def id(implicit ctx: Context) = {
      if (_id == 0) _id = ctx.nextId
      _id
    }

    /** The last denotation of this symbol */
    private[this] var lastDenot: SymDenotation = denotf(this)

    /** The current denotation of this symbol */
    final def denot(implicit ctx: Context): SymDenotation = {
      var denot = lastDenot
      if (!(denot.validFor contains ctx.period)) denot = denot.current.asInstanceOf[SymDenotation]
      denot
    }

    /** Subclass tests and casts */
    final def isTerm(implicit ctx: Context): Boolean = denot.isTerm
    final def isType(implicit ctx: Context): Boolean = denot.isType
    final def isClass: Boolean = isInstanceOf[ClassSymbol]

    final def asTerm(implicit ctx: Context): TermSymbol = { assert(isTerm, this); asInstanceOf[TermSymbol] }
    final def asType(implicit ctx: Context): TypeSymbol = { assert(isType, this); asInstanceOf[TypeSymbol] }
    final def asClass: ClassSymbol = asInstanceOf[ClassSymbol]

    /** A unique, densely packed integer tag for each class symbol, -1
     *  for all other symbols. To save memory, this method
     *  should be called only if class is a super class of some other class.
     */
    def superId: Int = -1

    final def entered(implicit ctx: Context): this.type = {
      this.owner.info.decls.enter(this)
      this
    }

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(implicit ctx: Context) = defn.ScalaValueClasses contains this

    /** Is symbol a phantom class for which no runtime representation exists? */
    def isPhantomClass(implicit ctx: Context) = defn.PhantomClasses contains this

    /** The current name of this symbol */
    final def name(implicit ctx: Context): ThisName = denot.name.asInstanceOf[ThisName]

    def show(implicit ctx: Context): String = ctx.show(this)
    def showLocated(implicit ctx: Context): String = ctx.showLocated(this)
    def showDcl(implicit ctx: Context): String = ctx.showDcl(this)
    def showKind(implicit ctx: Context): String = ctx.showKind(this)
    def showName(implicit ctx: Context): String = ctx.showName(this)
    def showFullName(implicit ctx: Context): String = ctx.showFullName(this)

  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol(off: Offset, denotf: ClassSymbol => ClassDenotation) extends Symbol(off, s => denotf(s.asClass)) {

    type ThisName = TypeName

    final def classDenot(implicit ctx: Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    private var superIdHint: Int = -1

    def superId(implicit ctx: Context): Int = {
      val hint = superIdHint
      val key = this.typeConstructor
      if (hint >= 0 && hint <= ctx.lastSuperId && (ctx.classOfId(hint) eq key))
        hint
      else {
        val id = ctx.superIdOfClass get key match {
          case Some(id) =>
            id
          case None =>
            val id = ctx.nextSuperId
            ctx.superIdOfClass(key) = id
            ctx.classOfId(id) = key
            id
        }
        superIdHint = id
        id
      }
    }
  }

  class ErrorSymbol(val underlying: Symbol, msg: => String)(implicit ctx: Context) extends Symbol(NoOffset, sym => underlying.denot) {
    type ThisName = underlying.ThisName
  }

  object NoSymbol extends Symbol(NoOffset, sym => NoDenotation) {
    override def exists = false
  }

  implicit class Copier[N <: Name](sym: Symbol { type ThisName = N })(implicit ctx: Context) {
    /** Copy a symbol, overriding selective fields */
    def copy(
        owner: Symbol = sym.owner,
        name: N = sym.name,
        flags: FlagSet = sym.flags,
        privateWithin: Symbol = sym.privateWithin,
        info: Type = sym.info,
        off: Offset = sym.offset): Symbol =
      if (sym.isClass) {
        assert(info eq sym.info)
        new ClassCopier(sym.asClass).copy(owner, name.asTypeName, flags, privateWithin = privateWithin, off = off)
      } else
        ctx.newSymbol(owner, name, flags, info, privateWithin, sym.offset)
  }

  implicit class ClassCopier(cls: ClassSymbol)(implicit ctx: Context) {
    /** Copy a class symbol, overriding selective fields */
    def copy(
        owner: Symbol = cls.owner,
        name: TypeName = cls.name.asTypeName,
        flags: FlagSet = cls.flags,
        parents: List[TypeRef] = cls.classDenot.parents,
        privateWithin: Symbol = cls.privateWithin,
        selfType: Type = cls.selfType,
        decls: Scope = cls.decls,
        associatedFile: AbstractFile = cls.associatedFile,
        off: Offset = cls.offset) =
      ctx.newClassSymbol(owner, name, flags, parents, privateWithin, selfType, decls, associatedFile, off)
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.definitions

  implicit def toFlagSet(sym: Symbol)(implicit ctx: Context): FlagSet = sym.flags

  implicit def toDenot(sym: Symbol)(implicit ctx: Context): SymDenotation = sym.denot

  implicit def toClassDenot(cls: ClassSymbol)(implicit ctx: Context): ClassDenotation = cls.classDenot

}
