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
import Types._, Annotations._
import Denotations.{ Denotation, SingleDenotation, MultiDenotation }
import collection.mutable
import io.AbstractFile

trait Symbols { this: Context =>

  def newLazyTermSymbol(owner: Symbol, name: TermName, initFlags: FlagSet, completer: SymCompleter) =
    new TermSymbol(new LazySymDenotation(_, owner, name, initFlags, completer))

  def newLazyTypeSymbol(owner: Symbol, name: TypeName, initFlags: FlagSet, completer: SymCompleter) =
    new TypeSymbol(new LazySymDenotation(_, owner, name, initFlags, completer))

  def newLazyClassSymbol(owner: Symbol, name: TypeName, initFlags: FlagSet, completer: ClassCompleter, assocFile: AbstractFile = null) =
    new ClassSymbol(new LazyClassDenotation(_, owner, name, initFlags, completer, assocFile)(this))

  def newLazyModuleSymbols(owner: Symbol,
      name: TermName,
      flags: FlagSet,
      completer: ClassCompleter,
      assocFile: AbstractFile = null)
  = {
    val module = newLazyTermSymbol(
      owner, name, flags | ModuleCreationFlags, new ModuleCompleter(condensed))
    val modcls = newLazyClassSymbol(
      owner, name.toTypeName, flags | ModuleClassCreationFlags, completer, assocFile)
    module.denot.asInstanceOf[LazySymDenotation].info =
      TypeRef(owner.thisType(ctx), modcls)
    modcls.denot.asInstanceOf[LazyClassDenotation].selfType =
      TermRef(owner.thisType, module)
    (module, modcls)
  }

  def newLazyPackageSymbols(owner: Symbol, name: TermName, completer: ClassCompleter) =
    newLazyModuleSymbols(owner, name, PackageCreationFlags, completer)

  def newTermSymbol(
    owner: Symbol,
    name: TermName,
    flags: FlagSet,
    info: Type,
    privateWithin: Symbol = NoSymbol)
  =
    new TermSymbol(new CompleteSymDenotation(_, owner, name, flags, privateWithin, info))

  def newTypeSymbol(
    owner: Symbol,
    name: TypeName,
    flags: FlagSet,
    info: Type,
    privateWithin: Symbol = NoSymbol)
  =
    new TypeSymbol(new CompleteSymDenotation(_, owner, name, flags, privateWithin, info))

  def newAliasTypeSymbol(owner: Symbol, name: TypeName, alias: Type, flags: FlagSet = EmptyFlags, privateWithin: Symbol = NoSymbol) =
    newTypeSymbol(owner, name, flags, TypeBounds(alias, alias), privateWithin)

  def newClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      privateWithin: Symbol = NoSymbol,
      optSelfType: Type = NoType,
      decls: Scope = newScope,
      assocFile: AbstractFile = null)
  =
    new ClassSymbol(new CompleteClassDenotation(
      _, owner, name, flags, privateWithin, parents, optSelfType, decls, assocFile)(this))

  def newModuleSymbols(
      owner: Symbol,
      name: TermName,
      flags: FlagSet,
      classFlags: FlagSet,
      parents: List[TypeRef],
      privateWithin: Symbol = NoSymbol,
      decls: Scope = newScope,
      assocFile: AbstractFile = null)(implicit ctx: Context)
  = {
    val module = newLazyTermSymbol(owner, name, flags | ModuleCreationFlags, new ModuleCompleter(condensed))
    val modcls = newClassSymbol(
      owner, name.toTypeName, classFlags | ModuleClassCreationFlags, parents, privateWithin,
      optSelfType = TermRef(owner.thisType, module),
      decls, assocFile)
    module.denot.asInstanceOf[LazySymDenotation].info =
      TypeRef(owner.thisType, modcls)
    (module, modcls)
  }

  def newPackageSymbols(
      owner: Symbol,
      name: TermName,
      decls: Scope = newScope)(implicit ctx: Context) =
   newModuleSymbols(
     owner, name, PackageCreationFlags, PackageCreationFlags, Nil, NoSymbol, decls)

  def newStubSymbol(owner: Symbol, name: Name)(implicit ctx: Context): Symbol = {
    def stub[Denot <: SymDenotation] = new StubCompleter[Denot](ctx.condensed)
    name match {
      case name: TermName => ctx.newLazyTermSymbol(owner, name, EmptyFlags, stub)
      case name: TypeName => ctx.newLazyClassSymbol(owner, name, EmptyFlags, stub)
    }
  }
}

object Symbols {

  /** A Symbol represents a Scala definition/declaration or a package.
   */
  abstract class Symbol(denotf: Symbol => SymDenotation) extends DotClass {

    type ThisName <: Name

    /** Is symbol different from NoSymbol? */
    def exists = true

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol) = if (exists) this else that

    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** The last denotation of this symbol */
    private[this] var lastDenot: SymDenotation = denotf(this)

    /** The current denotation of this symbol */
    final def denot(implicit ctx: Context): SymDenotation = {
      var denot = lastDenot
      if (!(denot.validFor contains ctx.period)) denot = denot.current.asInstanceOf[SymDenotation]
      denot
    }

    /** Subclass tests and casts */
    def isType: Boolean = false
    def isTerm: Boolean = false
    def isClass: Boolean = false

    def asTerm: TermSymbol = asInstanceOf[TermSymbol]
    def asType: TypeSymbol = asInstanceOf[TypeSymbol]
    def asClass: ClassSymbol = asInstanceOf[ClassSymbol]

    /** A unique, densely packed integer tag for each class symbol, -1
     *  for all other symbols. To save memory, this method
     *  should be called only if class is a super class of some other class.
     */
    def superId: Int = -1

    final def entered(implicit ctx: Context): this.type = {
      owner.info.decls.enter(this)
      this
    }

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(implicit ctx: Context) = defn.ScalaValueClasses contains this

    /** Is symbol a phantom class for which no runtime representation exists? */
    def isPhantomClass(implicit ctx: Context) = defn.PhantomClasses contains this

    // --------- Forwarders for sym methods --------------------------

    /** Special case tests for flags that are known a-priori and do not need loading
     *  flags.
     */
    def isModule(implicit ctx: Context) = denot.isModule
    def isModuleObj(implicit ctx: Context) = denot.isModuleVal
    def isModuleClass(implicit ctx: Context) = denot.isModuleClass
    def isPackage(implicit ctx: Context) = denot.isPackage
    def isPackageObj(implicit ctx: Context) = denot.isPackageVal
    def isPackageClass(implicit ctx: Context) = denot.isPackageClass

    /** The current owner of this symbol */
    final def owner(implicit ctx: Context): Symbol = denot.owner

    /** The current name of this symbol */
    final def name(implicit ctx: Context): ThisName = denot.name.asInstanceOf[ThisName]

    /** The current type info of this symbol */
    final def info(implicit ctx: Context): Type = denot.info

    /** The current flag set of this symbol */
    final def flags(implicit ctx: Context): FlagSet = denot.flags

    /** The current privateWithin boundary of this symbol, NoSymbol if no boundary is given. */
    final def privateWithin(implicit ctx: Context): Symbol = denot.privateWithin

    /** The current annotations of this symbol */
    final def annotations(implicit ctx: Context): List[Annotation] = denot.annotations

    /** Set given flags(s) of this symbol */
    def setFlag(flags: FlagSet)(implicit ctx: Context): Unit = denot.setFlag(flags)

    /** Unset given flag(s) of this symbol */
    def resetFlag(flags: FlagSet)(implicit ctx: Context): Unit = denot.resetFlag(flags)

    /** Does this symbol have an annotation matching the given class symbol? */
    final def hasAnnotation(cls: Symbol)(implicit ctx: Context): Boolean = denot.hasAnnotation(cls)

    /** Add given annotation to this symbol */
    final def addAnnotation(annot: Annotation)(implicit ctx: Context): Unit = denot.addAnnotation(annot)

    /** Record that this symbol is an alias of given `alias` symbol */
    final def setAlias(alias: Symbol)(implicit ctx: Context): Unit = denot.setAlias(alias)

    /** The chain of owners of this symbol, starting with the symbol itself */
    final def ownersIterator(implicit ctx: Context): Iterator[Symbol] = denot.ownersIterator

    /** Same as `ownersIterator contains sym` but more efficient. */
    final def hasTransOwner(sym: Symbol)(implicit ctx: Context): Boolean = denot.hasTransOwner(sym)

    /** The owner, skipping package objects. */
    def effectiveOwner(implicit ctx: Context) = denot.effectiveOwner

    /** If this is a package object or its implementing class, its owner: otherwise this.
     */
    def skipPackageObject(implicit ctx: Context): Symbol = if (this is PackageObject) owner else this

    /** The class containing this symbol */
    def enclosingClass(implicit ctx: Context): Symbol = denot.enclosingClass

    /** The top-level class containing this symbol, except for a toplevel module
     *  its module class
     */
    def topLevelClass(implicit ctx: Context): Symbol = denot.topLevelClass

    /** The package containing this symbol */
    def enclosingPackage(implicit ctx: Context): Symbol = denot.enclosingPackage

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Drops package objects.
     */
    final def fullName(separator: Char)(implicit ctx: Context): Name = denot.fullName(separator)
    final def fullName(implicit ctx: Context): Name = fullName('.')

    /** The source or class file from which this symbol was generated, null if not applicable. */
    final def associatedFile(implicit ctx: Context): AbstractFile = denot.associatedFile

    /** The class file from which this symbol was generated, null if not applicable. */
    final def binaryFile(implicit ctx: Context): AbstractFile = denot.binaryFile

    /** The source or class file from which this symbol was generated, null if not applicable. */
    final def sourceFile(implicit ctx: Context): AbstractFile = denot.sourceFile

    /** Is this symbol defined in the same compilation unit as that symbol? */
    final def isCoDefinedWith(that: Symbol)(implicit ctx: Context): Boolean = denot.isCoDefinedWith(that)

    /** The class with the same (type-) name as this module or module class,
     *  which is the defined in the same compilation unit.
     *  NoSymbol if this class does not exist.
     */
    final def companionClass(implicit ctx: Context): Symbol = denot.companionClass

    /** The module object with the same (term-) name as this class or module class,
     *  which is the defined in the same compilation unit.
     *  NoSymbol if this class does not exist.
     */
    final def companionModule(implicit ctx: Context): Symbol = denot.companionModule

    /** If this is a class, the module class of its companion object.
     *  If this is a module class, its companion class.
     *  NoSymbol otherwise.
     */
    final def linkedClass(implicit ctx: Context): Symbol = denot.linkedClass

    /** Is this symbol a subclass of the given class? */
    final def isSubClass(cls: Symbol)(implicit ctx: Context): Boolean = denot.isSubClass(cls)

    /** Is this class symbol a subclass of `cls`,
     *  and is this class symbol also different from Null or Nothing?
     */
    final def isNonBottomSubClass(cls: Symbol)(implicit ctx: Context): Boolean = denot.isNonBottomSubClass(cls)

    /** Is this symbol a subclass of `base` or a companion object of such a subclass? */
    final def isSubClassOrCompanion(base: Symbol)(implicit ctx: Context): Boolean = denot.isSubClassOrCompanion(base)

    /** The class that encloses the owner of the current context
     *  and that is a subclass of this class.
     */
    final def enclosingSubClass(implicit ctx: Context) = denot.enclosingSubClass

    ///** Is this symbol a proper subclass of the given class? */
    //def isProperSubClass(cls: ClassSymbol)(implicit ctx: Context): Boolean = (this ne cls) && this.isSubClass(cls)

    /** The non-private symbol whose type matches the type of this symbol
     *  in in given class.
     *
     *  @param inClass   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
    final def matchingSymbol(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = denot.matchingSymbol(inClass, site)

    /** The symbol, in class `inClass`, that is overridden by this symbol. */
    final def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol = denot.overriddenSymbol(inClass)

    /** All symbols overriden by this symbol. */
    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] = denot.allOverriddenSymbols

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base
     */
    final def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = denot.accessBoundary(base)

    /** Is this symbol contained in `boundary`? */
    final def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean = denot.isContainedIn(boundary)

    /** Is this symbol accessible whenever `that` symbol is accessible?
     *  Does not take into account status of protected members.
     */
    final def isAsAccessibleAs(that: Symbol)(implicit ctx: Context): Boolean = denot.isAsAccessibleAs(that)

    /** Is this symbol a non-value class? */
    final def isNonValueClass(implicit ctx: Context): Boolean = denot.isNonValueClass

    /** Is this symbol accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     */
    final def isAccessibleFrom(pre: Type, superAccess: Boolean = false)(implicit ctx: Context): Boolean = denot.isAccessibleFrom(pre, superAccess)

    def show(implicit ctx: Context): String = ctx.show(this)
    def showLocated(implicit ctx: Context): String = ctx.showLocated(this)
    def showDef(implicit ctx: Context): String = ctx.showDef(this)
    def showKind(implicit tcx: Context): String = ???
    def showName(implicit ctx: Context): String = ???

    /** The type parameters of a class symbol, Nil for all other symbols */
    def typeParams(implicit ctx: Context): List[TypeSymbol] = denot.typeParams

    /** The type This(cls), where cls is this class symbol */
    def thisType(implicit ctx: Context): Type = denot.thisType

    /** Is this symbol the root class or its companion object? */
    def isRoot(implicit ctx: Context): Boolean = denot.isRoot

    /** Is this symbol the empty package class or its companion object? */
    def isEmptyPackage(implicit ctx: Context): Boolean = denot.isEmptyPackage

    /** Is this symbol an anonymous class? */
    def isAnonymousClass(implicit ctx: Context): Boolean = denot.isAnonymousClass

    /** Is this symbol the root class, empty package class, or one of their companion objects? */
    def isEffectiveRoot(implicit ctx: Context): Boolean = denot.isEffectiveRoot

    /** If this is a module symbol, the class defining its template, otherwise NoSymbol. */
    def moduleClass(implicit ctx: Context): Symbol = denot.moduleClass

    /** A copy of this symbol with the same denotation */
    def copy(implicit ctx: Context): Symbol = unsupported("copy")

    /** A copy of this symbol with the same denotation but a new owner */
    def copy(owner: Symbol)(implicit ctx: Context): Symbol = unsupported("copy")

    /** Can a term with this symbol be a stable value? */
    def isStable(implicit ctx: Context): Boolean = denot.isStable

    /** Is this symbol static (i.e. with no outer instance)? */
    def isStatic(implicit ctx: Context): Boolean = denot.isStatic

    /** Does this symbol denote a class that defines static symbols? */
    final def isStaticOwner(implicit ctx: Context): Boolean = denot.isStaticOwner

    //    def isOverridable: Boolean = !!! need to enforce that classes cannot be redefined

    //    def isSkolem: Boolean = ???

    //    def isAbstractType: Boolean = ???
    //    def newAbstractType(name: TypeName, info: TypeBounds): TypeSymbol = ???
    //    def newAbstractTerm(name: TermName, tpe: Type): TypeSymbol = ???

    //def isMethod(implicit ctx: Context): Boolean = denot.isMethod

  }

  class TermSymbol(denotf: Symbol => SymDenotation) extends Symbol(denotf) {
    type ThisName = TermName
    override def isTerm = true
    override def copy(implicit ctx: Context): TermSymbol = copy(owner)
    override def copy(owner: Symbol)(implicit ctx: Context): TermSymbol = new TermSymbol(denot.copy(_, owner))
  }

  class TypeSymbol(denotf: Symbol => SymDenotation) extends Symbol(denotf) {
    type ThisName = TypeName
    override def isType = true
    override def copy(implicit ctx: Context): TypeSymbol = copy(owner)
    override def copy(owner: Symbol)(implicit ctx: Context): TypeSymbol = new TypeSymbol(denot.copy(_, owner))

    /** The type representing the type constructor for this type symbol */
    def typeConstructor(implicit ctx: Context): TypeRef = denot.typeConstructor

    /** The variance of this type parameter as an Int, with
     *  +1 = Covariant, -1 = Contravariant, 0 = Nonvariant
     */
    def variance(implicit ctx: Context): Int = denot.variance
  }

  class ClassSymbol(denotf: ClassSymbol => ClassDenotation) extends TypeSymbol(s => denotf(s.asClass)) {
    override def isClass = true
    override def copy(implicit ctx: Context): ClassSymbol = copy(owner)
    override def copy(owner: Symbol)(implicit ctx: Context): ClassSymbol = new ClassSymbol(classDenot.copyClass(_, owner))
    private var superIdHint: Int = -1

    final def classDenot(implicit ctx: Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    def selfType(implicit ctx: Context): Type = classDenot.selfType

    /** The base classes of this class in linearization order,
     *  with the class itself as first element.x
     */
    def baseClasses(implicit ctx: Context): List[ClassSymbol] = classDenot.baseClasses

    //    override def typeTemplate(implicit ctx: Context): Type = classDenot.typeTemplate

    def superId(implicit ctx: Context): Int = {
      val hint = superIdHint
      if (hint >= 0 && hint <= ctx.lastSuperId && (ctx.classOfId(hint) eq this)) hint
      else {
        val id = ctx.superIdOfClass get this match {
          case Some(id) =>
            id
          case None =>
            val id = ctx.nextSuperId
            ctx.superIdOfClass(this) = id
            ctx.classOfId(id) = this
            id
        }
        superIdHint = id
        id
      }
    }
  }

  class ErrorSymbol(underlying: Symbol, msg: => String)(implicit ctx: Context) extends Symbol(sym => underlying.denot) {
    override def isType = underlying.isType
    override def isTerm = underlying.isTerm
  }

  object NoSymbol extends Symbol(sym => NoDenotation) {
    override def exists = false
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.definitions

  implicit def toFlagSet(sym: Symbol)(implicit ctx: Context): FlagSet = sym.flags

}
