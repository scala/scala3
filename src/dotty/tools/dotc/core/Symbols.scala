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
import Types._, Annotations._, Positions._, StdNames._, Trees._
import Denotations.{ Denotation, SingleDenotation, MultiDenotation }
import collection.mutable
import io.AbstractFile

/** Creation methods for symbols */
trait Symbols { this: Context =>

// ---- Factory methods for symbol creation ----------------------
//
// All symbol creations should be done via the next two methods.

  /** Create a symbol without a denotation.
   *  Note this uses a cast instead of a direct type refinement because
   *  it's debug-friendlier not to create an anonymous class here.
   */
  def newNakedSymbol[N <: Name](coord: Coord = NoCoord): Symbol { type ThisName = N } =
    new Symbol(coord).asInstanceOf

  /** Create a class symbol without a denotation. */
  def newNakedClassSymbol(coord: Coord = NoCoord, assocFile: AbstractFile = null) =
    new ClassSymbol(coord, assocFile)

// ---- Symbol creation methods ----------------------------------

  /** Create a symbol from a function producing its denotation */
  def newSymbolDenoting[N <: Name](denotFn: Symbol => SymDenotation, coord: Coord = NoCoord): Symbol { type ThisName = N } = {
    val sym = newNakedSymbol[N](coord)
    sym.denot = denotFn(sym)
    sym
  }

  /** Create a symbol from its fields (info may be lazy) */
  def newSymbol[N <: Name](
      owner: Symbol,
      name: N,
      flags: FlagSet,
      info: Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord): Symbol { type ThisName = N } = {
    val sym = newNakedSymbol[N](coord)
    val denot = SymDenotation(sym, owner, name, flags, info, privateWithin)
    sym.denot = denot
    sym
  }

  /** Create a class symbol from a function producing its denotation */
  def newClassSymbolDenoting(denotFn: ClassSymbol => SymDenotation, coord: Coord = NoCoord, assocFile: AbstractFile = null): ClassSymbol = {
    val cls = newNakedClassSymbol(coord, assocFile)
    cls.denot = denotFn(cls)
    cls
  }

  /** Create a class symbol from its non-info fields and a function
   *  producing its info (the produced info may be lazy).
   */
  def newClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      infoFn: ClassSymbol => Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): ClassSymbol
  = {
    val cls = newNakedClassSymbol(coord, assocFile)
    val denot = SymDenotation(cls, owner, name, flags, infoFn(cls), privateWithin)
    cls.denot = denot
    cls
  }

  /** Create a class symbol from its non-info fields and the fields of its info. */
  def newCompleteClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      decls: Scope = newScope,
      optSelfType: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): ClassSymbol =
    newClassSymbol(
        owner, name, flags,
        ClassInfo(owner.thisType, _, parents, decls, optSelfType),
        privateWithin, coord, assocFile)

  /** Create a module symbol with associated module class
   *  from its non-info fields and a function producing the info
   *  of the module class (this info may be lazy).
   *  @param flags  The combined flags of the module and the module class
   *                These are masked with RetainedModuleFlags/RetainedModuleClassFlags.
   */
  def newModuleSymbol(
      owner: Symbol,
      name: TermName,
      flags: FlagSet,
      infoFn: (TermSymbol, ClassSymbol) => Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): TermSymbol
  = {
    val base = owner.thisType
    val module = newNakedSymbol[TermName](coord)
    val modcls = newNakedClassSymbol(coord, assocFile)
    val cdenot = SymDenotation(
        modcls, owner, name.toTypeName,
        flags & RetainedModuleClassFlags | ModuleClassCreationFlags,
        infoFn(module, modcls), privateWithin)
    val mdenot = SymDenotation(
        module, owner, name,
        flags & RetainedModuleValFlags | ModuleCreationFlags,
        if (cdenot.isCompleted) modcls.symbolicRef
        else new LazyModuleInfo(modcls)(condensed))
    module.denot = mdenot
    modcls.denot = cdenot
    module
  }

  /** Create a module symbol with associated module class
   *  from its non-info fields and the fields of the module class info.
   *  @param flags  The combined flags of the module and the module class
   *                These are masked with RetainedModuleValFlags/RetainedModuleClassFlags.
   */
  def newCompleteModuleSymbol(
      owner: Symbol,
      name: TermName,
      flags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): TermSymbol =
    newModuleSymbol(
        owner, name, flags,
        (module, modcls) => ClassInfo(
          owner.thisType, modcls, parents, decls, TermRef(owner.thisType, module)),
        privateWithin, coord, assocFile)

  /** Create a package symbol with associated package class
   *  from its non-info fields and a lazy type for loading the package's members.
   */
  def newPackageSymbol(
      owner: Symbol,
      name: TermName,
      info: LazyType): TermSymbol =
    newModuleSymbol(owner, name, PackageCreationFlags, info)

  /** Create a package symbol with associated package class
   *  from its non-info fields its member scope.
   */
  def newCompletePackageSymbol(
      owner: Symbol,
      name: TermName,
      flags: FlagSet = EmptyFlags,
      decls: Scope = newScope): TermSymbol =
    newCompleteModuleSymbol(owner, name, flags | PackageCreationFlags, Nil, decls)


  /** Create a stub symbol that will issue a missing reference error
   *  when attempted to be completed.
   */
  def newStubSymbol(owner: Symbol, name: Name, file: AbstractFile = null): Symbol = {
    def stub = new StubInfo()(condensed)
    name match {
      case name: TermName =>
        newModuleSymbol(owner, name, EmptyFlags, stub, assocFile = file)
      case name: TypeName =>
        newClassSymbol(owner, name, EmptyFlags, stub, assocFile = file)
    }
  }

  /** Create the local template dummy of given class `cls`. */
  def newLocalDummy(cls: Symbol, coord: Coord = NoCoord) =
    newSymbol(cls, nme.localDummyName(cls), EmptyFlags, NoType)

  /** Create an import symbol pointing back to given qualifier `expr`. */
  def newImportSymbol(expr: SharedTree[Type], coord: Coord = NoCoord) =
    newSymbol(NoSymbol, nme.IMPORT, EmptyFlags, ImportType(expr), coord = coord)

  /** Create a class constructor symbol for given class `cls`. */
  def newConstructor(cls: ClassSymbol, flags: FlagSet, paramNames: List[TermName], paramTypes: List[Type], privateWithin: Symbol = NoSymbol, coord: Coord = NoCoord) =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, MethodType(paramNames, paramTypes)(_ => cls.typeConstructor), privateWithin, coord)

  /** Create an empty default constructor symbol for given class `cls`. */
  def newDefaultConstructor(cls: ClassSymbol) =
    newConstructor(cls, EmptyFlags, Nil, Nil)

  /** Create a symbol representing a selftype declaration for class `cls`. */
  def newSelfSym(cls: ClassSymbol) =
    ctx.newSymbol(cls, nme.THIS, SyntheticArtifact, cls.classInfo.selfType)

  /** Create new type parameters with given owner, names, and flags.
   *  @param boundsFn  A function that, given type refs to the newly created
   *                   parameters returns a list of their bounds.
   */
  def newTypeParams(
    owner: Symbol,
    names: List[TypeName],
    flags: FlagSet,
    boundsFn: List[TypeRef] => List[Type]) = {
    val tparams = names map (_ => newNakedSymbol[TypeName](NoCoord))
    val bounds = boundsFn(tparams map (_.symbolicRef))
    (names, tparams, bounds).zipped foreach { (name, tparam, bound) =>
      tparam.denot = SymDenotation(tparam, owner, name, flags | TypeParamCreationFlags, bound)
    }
    tparams
  }

  type OwnerMap = Symbol => Symbol

  /** Map given symbols, subjecting all types to given type map and owner map.
   *  Cross symbol references are brought over from originals to copies.
   *  Do not copy any symbols if all attributes of all symbols stay the same.
   */
  def mapSymbols(
      originals: List[Symbol],
      typeMap: TypeMap = IdentityTypeMap,
      ownerMap: OwnerMap = identity)
  =
    if (originals forall (sym =>
        (typeMap(sym.info) eq sym.info) && (ownerMap(sym.owner) eq sym.owner)))
      originals
    else {
      val copies: List[Symbol] = for (original <- originals) yield
        newNakedSymbol[original.ThisName](original.coord)
      val treeMap = new TypedTrees.TreeMapper(typeMap, ownerMap)
        .withSubstitution(originals, copies)
      (originals, copies).zipped foreach {(original, copy) =>
        val odenot = original.denot
        copy.denot = odenot.copySymDenotation(
          symbol = copy,
          owner = treeMap.ownerMap(odenot.owner),
          info = treeMap.typeMap(odenot.info),
          privateWithin = ownerMap(odenot.privateWithin),
          annotations = odenot.annotations.mapConserve(treeMap.apply))
      }
      copies
    }

// ----- Locating predefined symbols ----------------------------------------

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
  class Symbol private[Symbols] (val coord: Coord) extends DotClass {

    type ThisName <: Name

    private[this] var _id: Int = _

    /** The unique id of this symbol */
    def id(implicit ctx: Context) = {
      if (_id == 0) _id = ctx.nextId
      _id
    }

    /** The last denotation of this symbol */
    private[this] var lastDenot: SymDenotation = _

    /** Set the denotation of this symbol */
    private[Symbols] def denot_=(d: SymDenotation) =
      lastDenot = d

    /** The current denotation of this symbol */
    final def denot(implicit ctx: Context): SymDenotation = {
      var denot = lastDenot
      if (!(denot.validFor contains ctx.period))
        denot = denot.current.asInstanceOf[SymDenotation]
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

    /** This symbol entered into owner's scope (owner must be a class). */
    final def entered(implicit ctx: Context): this.type = {
      this.owner.asClass.enter(this)
      this
    }

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol)(implicit ctx: Context) =
      if (this.exists) this else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(implicit ctx: Context) = defn.ScalaValueClasses contains this

    /** Is symbol a phantom class for which no runtime representation exists? */
    def isPhantomClass(implicit ctx: Context) = defn.PhantomClasses contains this

    /** The current name of this symbol */
    final def name(implicit ctx: Context): ThisName = denot.name.asInstanceOf[ThisName]

    /** The source or class file from which this class or
     *  the class containing this symbol was generated, null if not applicable.
     */
    def associatedFile(implicit ctx: Context): AbstractFile =
      denot.topLevelClass.symbol.associatedFile

    /** The class file from which this class was generated, null if not applicable. */
    final def binaryFile(implicit ctx: Context): AbstractFile =
      pickFile(associatedFile, classFile = true)

    /** The source file from which this class was generated, null if not applicable. */
    final def sourceFile(implicit ctx: Context): AbstractFile =
      pickFile(associatedFile, classFile = false)

   /** Desire to re-use the field in ClassSymbol which stores the source
     *  file to also store the classfile, but without changing the behavior
     *  of sourceFile (which is expected at least in the IDE only to
     *  return actual source code.) So sourceFile has classfiles filtered out.
     */
    private def pickFile(file: AbstractFile, classFile: Boolean): AbstractFile =
      if ((file eq null) || classFile != (file.path endsWith ".class")) null else file

    def show(implicit ctx: Context): String = ctx.show(this)
    def showLocated(implicit ctx: Context): String = ctx.showLocated(this)
    def showDcl(implicit ctx: Context): String = ctx.showDcl(this)
    def showKind(implicit ctx: Context): String = ctx.showKind(this)
    def showName(implicit ctx: Context): String = ctx.showName(this)
    def showFullName(implicit ctx: Context): String = ctx.showFullName(this)
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, assocFile: AbstractFile)
    extends Symbol(coord) {

    type ThisName = TypeName

    /** The source or class file from which this class was generated, null if not applicable. */
    override def associatedFile(implicit ctx: Context): AbstractFile =
      if (this.owner.isPackageClass) assocFile
      else super.associatedFile

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

    /** Have we seen a subclass of this class? */
    def hasChildren = superIdHint >= 0
  }

  class ErrorSymbol(val underlying: Symbol, msg: => String)(implicit ctx: Context) extends Symbol(NoCoord) {
    type ThisName = underlying.ThisName
    denot = underlying.denot
  }

  object NoSymbol extends Symbol(NoCoord) {
    denot = NoDenotation
  }

  implicit class Copier[N <: Name](sym: Symbol { type ThisName = N })(implicit ctx: Context) {
    /** Copy a symbol, overriding selective fields */
    def copy(
        owner: Symbol = sym.owner,
        name: N = sym.name,
        flags: FlagSet = sym.flags,
        info: Type = sym.info,
        privateWithin: Symbol = sym.privateWithin,
        coord: Coord = sym.coord,
        associatedFile: AbstractFile = sym.associatedFile): Symbol =
      if (sym.isClass)
        ctx.newClassSymbol(owner, name.asTypeName, flags, _ => info, privateWithin, coord, associatedFile)
      else
        ctx.newSymbol(owner, name, flags, info, privateWithin, coord)
  }

  implicit def defn(implicit ctx: Context): Definitions = ctx.definitions

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(implicit ctx: Context): SymDenotation = sym.denot

  /** Makes all class denotations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(implicit ctx: Context): ClassDenotation = cls.classDenot
}
