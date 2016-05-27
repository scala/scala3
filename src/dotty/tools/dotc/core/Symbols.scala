package dotty.tools
package dotc
package core

import Periods._
import Names._
import Scopes._
import Flags._
import java.lang.AssertionError
import Decorators._
import Symbols._
import Contexts._
import SymDenotations._
import printing.Texts._
import printing.Printer
import Types._
import Annotations._
import util.Positions._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.tpd.Tree
import ast.TreeTypeMap
import Constants.Constant
import Denotations.{ Denotation, SingleDenotation, MultiDenotation }
import collection.mutable
import io.AbstractFile
import language.implicitConversions
import util.{NoSource, DotClass}

/** Creation methods for symbols */
trait Symbols { this: Context =>

// ---- Factory methods for symbol creation ----------------------
//
// All symbol creations should be done via the next two methods.

  /** Create a symbol without a denotation.
   *  Note this uses a cast instead of a direct type refinement because
   *  it's debug-friendlier not to create an anonymous class here.
   */
  def newNakedSymbol[N <: Name](coord: Coord = NoCoord)(implicit ctx: Context): Symbol { type ThisName = N } =
    new Symbol(coord, ctx.nextId).asInstanceOf[Symbol { type ThisName = N }]

  /** Create a class symbol without a denotation. */
  def newNakedClassSymbol(coord: Coord = NoCoord, assocFile: AbstractFile = null)(implicit ctx: Context) =
    new ClassSymbol(coord, assocFile, ctx.nextId)

// ---- Symbol creation methods ----------------------------------

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
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): ClassSymbol =
    newClassSymbol(
        owner, name, flags,
        ClassInfo(owner.thisType, _, parents, decls, selfInfo),
        privateWithin, coord, assocFile)

  /** Same as `newCompleteClassSymbol` except that `parents` can be a list of arbitrary
   *  types which get normalized into type refs and parameter bindings.
   */
  def newNormalizedClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parentTypes: List[Type],
      decls: Scope = newScope,
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): ClassSymbol = {
    def completer = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        val parentRefs: List[TypeRef] = normalizeToClassRefs(parentTypes, cls, decls)
        denot.info = ClassInfo(owner.thisType, cls, parentRefs, decls)
      }
    }
    newClassSymbol(owner, name, flags, completer, privateWithin, coord, assocFile)
  }

  /** Create a module symbol with associated module class
   *  from its non-info fields and a function producing the info
   *  of the module class (this info may be lazy).
   */
  def newModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      infoFn: (TermSymbol, ClassSymbol) => Type, // typically a ModuleClassCompleterWithDecls
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): TermSymbol
  = {
    val base = owner.thisType
    val module = newNakedSymbol[TermName](coord)
    val modcls = newNakedClassSymbol(coord, assocFile)
    val modclsFlags = clsFlags | ModuleClassCreationFlags
    val modclsName = name.toTypeName.adjustIfModuleClass(modclsFlags)
    val cdenot = SymDenotation(
        modcls, owner, modclsName, modclsFlags,
        infoFn(module, modcls), privateWithin)
    val mdenot = SymDenotation(
        module, owner, name, modFlags | ModuleCreationFlags,
        if (cdenot.isCompleted) TypeRef.withSymAndName(owner.thisType, modcls, modclsName)
        else new ModuleCompleter(modcls))
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
      modFlags: FlagSet,
      clsFlags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null): TermSymbol =
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => ClassInfo(
          owner.thisType, modcls, parents, decls, TermRef.withSymAndName(owner.thisType, module, name)),
        privateWithin, coord, assocFile)

  val companionMethodFlags = Flags.Synthetic | Flags.Private | Flags.Method

  def synthesizeCompanionMethod(name: Name, target: SymDenotation, owner: SymDenotation)(implicit ctx: Context) =
    if (owner.exists && target.exists && !owner.isAbsent && !target.isAbsent) {
      val existing = owner.unforcedDecls.lookup(name)

      existing.orElse{
        ctx.newSymbol(owner.symbol, name, companionMethodFlags , ExprType(target.typeRef))
      }
    } else NoSymbol

  /** Create a package symbol with associated package class
   *  from its non-info fields and a lazy type for loading the package's members.
   */
  def newPackageSymbol(
      owner: Symbol,
      name: TermName,
      infoFn: (TermSymbol, ClassSymbol) => LazyType): TermSymbol =
    newModuleSymbol(owner, name, PackageCreationFlags, PackageCreationFlags, infoFn)

  /** Create a package symbol with associated package class
   *  from its non-info fields its member scope.
   */
  def newCompletePackageSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet = EmptyFlags,
      clsFlags: FlagSet = EmptyFlags,
      decls: Scope = newScope): TermSymbol =
    newCompleteModuleSymbol(
      owner, name,
      modFlags | PackageCreationFlags, clsFlags | PackageCreationFlags,
      Nil, decls)


  /** Create a stub symbol that will issue a missing reference error
   *  when attempted to be completed.
   */
  def newStubSymbol(owner: Symbol, name: Name, file: AbstractFile = null): Symbol = {
    def stubCompleter = new StubInfo()
    val normalizedOwner = if (owner is ModuleVal) owner.moduleClass else owner
    println(s"creating stub for ${name.show}, owner = ${normalizedOwner.denot.debugString}, file = $file")
    println(s"decls = ${normalizedOwner.unforcedDecls.toList.map(_.debugString).mkString("\n  ")}") // !!! DEBUG
    //if (base.settings.debug.value) throw new Error()
    val stub = name match {
      case name: TermName =>
        newModuleSymbol(normalizedOwner, name, EmptyFlags, EmptyFlags, stubCompleter, assocFile = file)
      case name: TypeName =>
        newClassSymbol(normalizedOwner, name, EmptyFlags, stubCompleter, assocFile = file)
    }
    stubs = stub :: stubs
    stub
  }

  /** Create the local template dummy of given class `cls`.
   *  In a template
   *
   *     trait T { val fld: Int; { val x: int = 2 }; val fld2 = { val y = 2; y }}
   *
   *  the owner of `x` is the local dummy of the template. The owner of the local
   *  dummy is then the class of the template itself. By contrast, the owner of `y`
   *  would be `fld2`. There is a single local dummy per template.
   */
  def newLocalDummy(cls: Symbol, coord: Coord = NoCoord) =
    newSymbol(cls, nme.localDummyName(cls), EmptyFlags, NoType)

  /** Create an import symbol pointing back to given qualifier `expr`. */
  def newImportSymbol(owner: Symbol, expr: Tree, coord: Coord = NoCoord) =
    newSymbol(owner, nme.IMPORT, EmptyFlags, ImportType(expr), coord = coord)

  /** Create a class constructor symbol for given class `cls`. */
  def newConstructor(cls: ClassSymbol, flags: FlagSet, paramNames: List[TermName], paramTypes: List[Type], privateWithin: Symbol = NoSymbol, coord: Coord = NoCoord) =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, MethodType(paramNames, paramTypes)(_ => cls.typeRef), privateWithin, coord)

  /** Create an empty default constructor symbol for given class `cls`. */
  def newDefaultConstructor(cls: ClassSymbol) =
    newConstructor(cls, EmptyFlags, Nil, Nil)

  /** Create a symbol representing a selftype declaration for class `cls`. */
  def newSelfSym(cls: ClassSymbol, name: TermName = nme.WILDCARD, selfInfo: Type = NoType): TermSymbol =
    ctx.newSymbol(cls, name, SelfSymFlags, selfInfo orElse cls.classInfo.selfType, coord = cls.coord)

  /** Create new type parameters with given owner, names, and flags.
   *  @param boundsFn  A function that, given type refs to the newly created
   *                   parameters returns a list of their bounds.
   */
  def newTypeParams(
    owner: Symbol,
    names: List[TypeName],
    flags: FlagSet,
    boundsFn: List[TypeRef] => List[Type]): List[TypeSymbol] = {

    val tparamBuf = new mutable.ListBuffer[TypeSymbol]
    val trefBuf = new mutable.ListBuffer[TypeRef]
    for (name <- names) {
      val tparam = newNakedSymbol[TypeName](NoCoord)
      tparamBuf += tparam
      trefBuf += TypeRef.withSymAndName(owner.thisType, tparam, name)
    }
    val tparams = tparamBuf.toList
    val bounds = boundsFn(trefBuf.toList)
    for ((name, tparam, bound) <- (names, tparams, bounds).zipped)
      tparam.denot = SymDenotation(tparam, owner, name, flags | owner.typeParamCreationFlags, bound)
    tparams
  }

  /** Create a new skolem symbol. This is not the same as SkolemType, even though the
   *  motivation (create a singleton referencing to a type) is similar.
   */
  def newSkolem(tp: Type) = newSymbol(defn.RootClass, nme.SKOLEM, SyntheticArtifact | Permanent, tp)

  def newErrorSymbol(owner: Symbol, name: Name) =
    newSymbol(owner, name, SyntheticArtifact,
      if (name.isTypeName) TypeAlias(ErrorType) else ErrorType)

  /** Map given symbols, subjecting their attributes to the mappings
   *  defined in the given TreeTypeMap `ttmap`.
   *  Cross symbol references are brought over from originals to copies.
   *  Do not copy any symbols if all attributes of all symbols stay the same.
   */
  def mapSymbols(originals: List[Symbol], ttmap: TreeTypeMap, mapAlways: Boolean = false): List[Symbol] =
    if (originals.forall(sym =>
        (ttmap.mapType(sym.info) eq sym.info) &&
        !(ttmap.oldOwners contains sym.owner)) && !mapAlways)
      originals
    else {
      val copies: List[Symbol] = for (original <- originals) yield
        original match {
          case original: ClassSymbol =>
            newNakedClassSymbol(original.coord, original.assocFile)
          case _ =>
            newNakedSymbol[original.ThisName](original.coord)
        }
      val ttmap1 = ttmap.withSubstitution(originals, copies)
      (originals, copies).zipped foreach {(original, copy) =>
        copy.denot = original.denot // preliminary denotation, so that we can access symbols in subsequent transform
      }
      (originals, copies).zipped foreach {(original, copy) =>
        val odenot = original.denot
        val oinfo = original.info match {
          case ClassInfo(pre, _, parents, decls, selfInfo) =>
            assert(original.isClass)
            ClassInfo(pre, copy.asClass, parents, decls.cloneScope, selfInfo)
          case oinfo => oinfo
        }
        copy.denot = odenot.copySymDenotation(
          symbol = copy,
          owner = ttmap1.mapOwner(odenot.owner),
          initFlags = odenot.flags &~ Frozen | Fresh,
          info = ttmap1.mapType(oinfo),
          privateWithin = ttmap1.mapOwner(odenot.privateWithin), // since this refers to outer symbols, need not include copies (from->to) in ownermap here.
          annotations = odenot.annotations.mapConserve(ttmap1.apply))
      }
      copies
    }

// ----- Locating predefined symbols ----------------------------------------

  def requiredPackage(path: PreName): TermSymbol =
    base.staticRef(path.toTermName).requiredSymbol(_ is Package).asTerm

  def requiredPackageRef(path: PreName): TermRef = requiredPackage(path).termRef

  def requiredClass(path: PreName): ClassSymbol =
    base.staticRef(path.toTypeName).requiredSymbol(_.isClass).asClass

  def requiredClassRef(path: PreName): TypeRef = requiredClass(path).typeRef

  /** Get ClassSymbol if class is either defined in current compilation run
   *  or present on classpath.
   *  Returns NoSymbol otherwise. */
  def getClassIfDefined(path: PreName): Symbol =
    base.staticRef(path.toTypeName, generateStubs = false).requiredSymbol(_.isClass, generateStubs = false)

  def requiredModule(path: PreName): TermSymbol =
    base.staticRef(path.toTermName).requiredSymbol(_ is Module).asTerm

  def requiredModuleRef(path: PreName): TermRef = requiredModule(path).termRef
}

object Symbols {

  implicit def eqSymbol: Eq[Symbol, Symbol] = Eq

  /** A Symbol represents a Scala definition/declaration or a package.
   *  @param coord  The coordinates of the symbol (a position or an index)
   *  @param id     A unique identifier of the symbol (unique per ContextBase)
   */
  class Symbol private[Symbols] (val coord: Coord, val id: Int) extends DotClass with MemberBinding with printing.Showable {

    type ThisName <: Name

    //assert(id != 4285)

    /** The last denotation of this symbol */
    private[this] var lastDenot: SymDenotation = _

    /** Set the denotation of this symbol */
    private[core] def denot_=(d: SymDenotation) =
      lastDenot = d

    /** The current denotation of this symbol */
    final def denot(implicit ctx: Context): SymDenotation = {
      var denot = lastDenot
      if (!(denot.validFor contains ctx.period)) {
        denot = denot.current.asInstanceOf[SymDenotation]
        lastDenot = denot
      }
      denot
    }

    private[core] def defRunId: RunId =
      if (lastDenot == null) NoRunId else lastDenot.validFor.runId

    /** Does this symbol come from a currently compiled source file? */
    final def isDefinedInCurrentRun(implicit ctx: Context): Boolean = {
      pos.exists && defRunId == ctx.runId
    }

    /** Subclass tests and casts */
    final def isTerm(implicit ctx: Context): Boolean =
      (if (defRunId == ctx.runId) lastDenot else denot).isTerm

    final def isType(implicit ctx: Context): Boolean =
      (if (defRunId == ctx.runId) lastDenot else denot).isType

    final def isClass: Boolean = isInstanceOf[ClassSymbol]

    final def asTerm(implicit ctx: Context): TermSymbol = { assert(isTerm, s"asTerm called on not-a-Term $this" ); asInstanceOf[TermSymbol] }
    final def asType(implicit ctx: Context): TypeSymbol = { assert(isType, s"isType called on not-a-Type $this"); asInstanceOf[TypeSymbol] }
    final def asClass: ClassSymbol = asInstanceOf[ClassSymbol]

    final def isFresh(implicit ctx: Context) =
      lastDenot != null && (lastDenot is Fresh)

    /** Special cased here, because it may be used on naked symbols in substituters */
    final def isStatic(implicit ctx: Context): Boolean =
      lastDenot != null && denot.isStatic

    /** A unique, densely packed integer tag for each class symbol, -1
     *  for all other symbols. To save memory, this method
     *  should be called only if class is a super class of some other class.
     */
    def superId(implicit ctx: Context): Int = -1

    /** This symbol entered into owner's scope (owner must be a class). */
    final def entered(implicit ctx: Context): this.type = {
      assert(this.owner.isClass, s"symbol ($this) entered the scope of non-class owner ${this.owner}") // !!! DEBUG
      this.owner.asClass.enter(this)
      if (this is Module) this.owner.asClass.enter(this.moduleClass)
      this
    }

    /** Enter this symbol in its class owner after given `phase`. Create a fresh
     *  denotation for its owner class if the class has not yet already one
     *  that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def enteredAfter(phase: DenotTransformer)(implicit ctx: Context): this.type =
      if (ctx.phaseId != phase.next.id) enteredAfter(phase)(ctx.withPhase(phase.next))
      else {
        if (this.owner.is(Package)) {
          denot.validFor |= InitialPeriod
          if (this is Module) this.moduleClass.validFor |= InitialPeriod
        }
        else this.owner.asClass.ensureFreshScopeAfter(phase)
        entered
      }

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol)(implicit ctx: Context) =
      if (this.exists) this else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** The current name of this symbol */
    final def name(implicit ctx: Context): ThisName = denot.name.asInstanceOf[ThisName]

    /** The source or class file from which this class or
     *  the class containing this symbol was generated, null if not applicable.
     *  Overridden in ClassSymbol
     */
    def associatedFile(implicit ctx: Context): AbstractFile =
      denot.topLevelClass.symbol.associatedFile

    /** The class file from which this class was generated, null if not applicable. */
    final def binaryFile(implicit ctx: Context): AbstractFile = {
      val file = associatedFile
      if (file != null && file.path.endsWith("class")) file else null
    }

    /** The source file from which this class was generated, null if not applicable. */
    final def sourceFile(implicit ctx: Context): AbstractFile = {
      val file = associatedFile
      if (file != null && !file.path.endsWith("class")) file
      else denot.topLevelClass.getAnnotation(defn.SourceFileAnnot) match {
        case Some(sourceAnnot) => sourceAnnot.argumentConstant(0) match {
          case Some(Constant(path: String)) => AbstractFile.getFile(path)
          case none => null
        }
        case none => null
      }
    }

    /** The position of this symbol, or NoPosition is symbol was not loaded
     *  from source.
     */
    def pos: Position = if (coord.isPosition) coord.toPosition else NoPosition

    // MemberBinding methods
    def isTypeParam(implicit ctx: Context) = denot.is(TypeParam)
    def memberName(implicit ctx: Context): Name = name
    def memberBounds(implicit ctx: Context) = denot.info.bounds
    def memberBoundsAsSeenFrom(pre: Type)(implicit ctx: Context) = pre.memberInfo(this).bounds
    def memberVariance(implicit ctx: Context) = denot.variance

// -------- Printing --------------------------------------------------------

    /** The prefix string to be used when displaying this symbol without denotation */
    protected def prefixString = "Symbol"

    override def toString: String =
      if (lastDenot == null) s"Naked$prefixString#$id"
      else lastDenot.toString// + "#" + id // !!! DEBUG

    def toText(printer: Printer): Text = printer.toText(this)

    def showLocated(implicit ctx: Context): String = ctx.locatedText(this).show
    def showDcl(implicit ctx: Context): String = ctx.dclText(this).show
    def showKind(implicit ctx: Context): String = ctx.kindString(this)
    def showName(implicit ctx: Context): String = ctx.nameString(this)
    def showFullName(implicit ctx: Context): String = ctx.fullNameString(this)

    override def hashCode() = id // for debugging.
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, val assocFile: AbstractFile, id: Int)
    extends Symbol(coord, id) {

    type ThisName = TypeName

    /** The source or class file from which this class was generated, null if not applicable. */
    override def associatedFile(implicit ctx: Context): AbstractFile =
      if (assocFile != null || (this.owner is PackageClass) || this.isEffectiveRoot) assocFile
      else super.associatedFile

    final def classDenot(implicit ctx: Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    private var superIdHint: Int = -1

    override def superId(implicit ctx: Context): Int = {
      val hint = superIdHint
      if (hint >= 0 && hint <= ctx.lastSuperId && (ctx.classOfId(hint) eq this))
        hint
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

    override protected def prefixString = "ClassSymbol"
  }

  class ErrorSymbol(val underlying: Symbol, msg: => String)(implicit ctx: Context) extends Symbol(NoCoord, ctx.nextId) {
    type ThisName = underlying.ThisName
    denot = underlying.denot
  }

  @sharable object NoSymbol extends Symbol(NoCoord, 0) {
    denot = NoDenotation

    override def associatedFile(implicit ctx: Context): AbstractFile = NoSource.file
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

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(implicit ctx: Context): SymDenotation = sym.denot

  /** Makes all class denotations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(implicit ctx: Context): ClassDenotation = cls.classDenot

  /** The Definitions object */
  def defn(implicit ctx: Context): Definitions = ctx.definitions

  /** The current class */
  def currentClass(implicit ctx: Context): ClassSymbol = ctx.owner.enclosingClass.asClass

  @sharable var stubs: List[Symbol] = Nil // diagnostic only
}
