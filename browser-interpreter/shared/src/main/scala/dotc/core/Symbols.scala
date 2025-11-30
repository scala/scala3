package dotc.core

import Names._
import Flags._
import Types._

import scala.collection.mutable

/**
 * Cross-platform symbol representation for the browser compiler.
 */
object Symbols {

  /** A unique identifier for symbols */
  private var nextId: Int = 0
  private def freshId(): Int = { nextId += 1; nextId }

  /** Base class for all symbols */
  abstract class Symbol {
    val id: Int = freshId()

    /** The name of this symbol */
    def name: Name

    /** The flags of this symbol */
    var flags: FlagSet = EmptyFlags

    /** The type of this symbol */
    var info: Type = NoType

    /** The owner of this symbol */
    var owner: Symbol = NoSymbol

    /** Private within qualifier */
    var privateWithin: Symbol = NoSymbol

    /** Annotations */
    var annotations: List[Any] = Nil

    /** Does this symbol exist? */
    def exists: Boolean = true

    /** Is this a type symbol? */
    def isType: Boolean = false

    /** Is this a term symbol? */
    def isTerm: Boolean = !isType

    /** Is this a class symbol? */
    def isClass: Boolean = false

    /** Is this a module (object) symbol? */
    def isModule: Boolean = flags.is(Module)

    /** Is this a package symbol? */
    def isPackage: Boolean = flags.is(Package)

    /** Is this a method symbol? */
    def isMethod: Boolean = flags.is(Method)

    /** Is this a val/var symbol? */
    def isValue: Boolean = isTerm && !isMethod && !isModule

    /** Is this a type parameter? */
    def isTypeParam: Boolean = isType && flags.is(Param)

    /** Check if flag is set */
    def is(flag: Flag): Boolean = flags.is(flag)
    def is(flag: Flag, butNot: FlagSet): Boolean = flags.is(flag, butNot)
    def isOneOf(fs: FlagSet): Boolean = flags.isOneOf(fs)
    def isAllOf(fs: FlagSet): Boolean = flags.isAllOf(fs)

    /** Set flags */
    def setFlag(flag: Flag): this.type = { flags = flags | flag; this }
    def resetFlag(flag: Flag): this.type = { flags = flags &~ flag; this }

    /** The denotation of this symbol */
    def denot: Denotation = SingleDenotation(this, info)

    /** The type of this symbol's definition site */
    def typeRef: TypeRef = TypeRef(owner.thisType, name.toTypeName)
    def termRef: TermRef = TermRef(owner.thisType, name.toTermName)

    /** The this-type of this symbol */
    def thisType: Type = ThisType(this)

    /** The primary constructor, if this is a class */
    def primaryConstructor: Symbol = NoSymbol

    /** The companion module/class */
    def companionModule: Symbol = NoSymbol
    def companionClass: Symbol = NoSymbol

    /** Full name including owner chain */
    def fullName: Name = {
      if (owner == NoSymbol || owner.isPackage && owner.name.toString == "<root>") name
      else qualifiedName(owner.fullName.toTermName, name.asSimpleName, ".")
    }

    /** For debugging */
    override def toString: String = s"${getClass.getSimpleName}($name, $flags)"
  }

  /** No symbol */
  object NoSymbol extends Symbol {
    def name: Name = EmptyTermName
    override def exists: Boolean = false
    override def toString: String = "NoSymbol"
  }

  /** A term symbol (value, method, module) */
  class TermSymbol(val name: TermName) extends Symbol {
    override def isTerm: Boolean = true
  }

  /** A type symbol (class, type alias, type parameter) */
  class TypeSymbol(val name: TypeName) extends Symbol {
    override def isType: Boolean = true
  }

  /** A class symbol */
  class ClassSymbol(name: TypeName) extends TypeSymbol(name) {
    override def isClass: Boolean = true

    /** The scope containing this class's members */
    var decls: Scope = Scope.empty

    /** The class's type parameters */
    var typeParams: List[TypeSymbol] = Nil

    /** The class's parent types */
    var parents: List[Type] = Nil

    /** The class's self type */
    var selfType: Type = NoType

    /** The class's primary constructor */
    private var _primaryConstructor: Symbol = NoSymbol
    override def primaryConstructor: Symbol = _primaryConstructor
    def setPrimaryConstructor(constr: Symbol): Unit = _primaryConstructor = constr

    /** Enter a member into this class's declarations */
    def enter(sym: Symbol): Unit = {
      sym.owner = this
      decls.enter(sym)
    }

    /** Lookup a member by name */
    def member(name: Name): Denotation = decls.lookupEntry(name)

    /** The class info type - computed from class data */
    def classInfo: Type = ClassInfo(owner.thisType, this, parents, decls)
  }

  /** A package symbol */
  class PackageSymbol(name: TermName) extends TermSymbol(name) {
    flags = Package

    /** The package's members */
    var decls: Scope = Scope.empty

    /** Enter a member into this package */
    def enter(sym: Symbol): Unit = {
      sym.owner = this
      decls.enter(sym)
    }

    /** Lookup a member by name */
    def member(name: Name): Denotation = decls.lookupEntry(name)

    /** The package info type - computed from package data */
    def packageInfo: Type = PackageInfo(this)
  }

  // ============= Symbol creation =============

  def newTermSymbol(owner: Symbol, name: TermName, flags: FlagSet = EmptyFlags): TermSymbol = {
    val sym = new TermSymbol(name)
    sym.owner = owner
    sym.flags = flags
    sym
  }

  def newTypeSymbol(owner: Symbol, name: TypeName, flags: FlagSet = EmptyFlags): TypeSymbol = {
    val sym = new TypeSymbol(name)
    sym.owner = owner
    sym.flags = flags
    sym
  }

  def newClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet = EmptyFlags): ClassSymbol = {
    val sym = new ClassSymbol(name)
    sym.owner = owner
    sym.flags = flags
    sym
  }

  def newPackageSymbol(owner: Symbol, name: TermName): PackageSymbol = {
    val sym = new PackageSymbol(name)
    sym.owner = owner
    sym
  }
}

// Re-export for convenience
val NoSymbol: Symbols.Symbol = Symbols.NoSymbol
type Symbol = Symbols.Symbol

