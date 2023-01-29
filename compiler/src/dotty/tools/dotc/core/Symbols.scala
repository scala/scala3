package dotty.tools
package dotc
package core

import Periods._
import Names._
import Scopes._
import Flags._
import Decorators._
import Contexts._
import Phases._
import SymDenotations._
import Denotations._
import printing.Texts._
import printing.Printer
import Types._
import util.Spans._
import DenotTransformers._
import StdNames._
import NameOps._
import transform.SymUtils._
import NameKinds.LazyImplicitName
import Annotations.Annotation
import ast.tpd
import tpd.{Tree, TreeProvider, TreeOps, EmptyTree, NameTree}
import ast.TreeTypeMap
import Constants.Constant
import Variances.Variance
import reporting.Message
import collection.mutable
import io.AbstractFile
import util.{SourceFile, NoSource, Property, SourcePosition, SrcPos, EqHashMap}
import scala.annotation.internal.sharable
import config.Printers.typr
import annotation.tailrec
import scala.reflect.TypeTest

object Symbols {

  implicit def eqSymbol: CanEqual[Symbol, Symbol] = CanEqual.derived

  /** Tree attachment containing the identifiers in a tree as a sorted array */
  val Ids: Property.Key[Array[String]] = new Property.Key

  opaque type Symbl <: ParamInfo & SrcPos & Named & printing.Showable
    = SymDenotation

  opaque type ClassSymbl <: Symbl
    = ClassDenotation

  object TypeTests:

    given SymTest: TypeTest[AnyRef, Symbol] with
      def unapply(x: AnyRef): Option[x.type & Symbol] = x match
        case sd: SymDenotation => Some(sd.asInstanceOf)
        case _ => None

    given ClsTest: TypeTest[AnyRef, ClassSymbol] with
      def unapply(x: AnyRef): Option[x.type & ClassSymbol] = x match
        case cd: ClassDenotation => Some(cd.asInstanceOf)
        case _ => None
  end TypeTests

  /** A Symbol represents a Scala definition/declaration or a package.
   *  @param coord  The coordinates of the symbol (a position or an index)
   *  @param id     A unique identifier of the symbol (unique per ContextBase)
   */
  class Symbol private[Symbols] ()
    extends ParamInfo, SrcPos, Named, printing.Showable {

    util.Stats.record(s"new ${getClass}")

    var initialDenot: SymDenotation = _

    def isClass: Boolean = isInstanceOf[ClassSymbol]

    // SrcPos types and methods
    final def span: Span = initialDenot.span
    final def sourcePos(using Context): SourcePosition = initialDenot.sourcePos
    final def srcPos: SrcPos = initialDenot.srcPos

    // ParamInfo types and methods
    def isTypeParam(using Context): Boolean = this.denot.is(TypeParam)
    def paramName(using Context): ThisName = this.denot.paramName.asInstanceOf
    def paramInfo(using Context): Type = this.denot.info
    def paramInfoAsSeenFrom(pre: Type)(using Context): Type = this.denot.paramInfoAsSeenFrom(pre)
    def paramInfoOrCompleter(using Context): Type = this.denot.paramInfoOrCompleter
    def paramVariance(using Context): Variance = this.denot.paramVariance
    def paramRef(using Context): TypeRef = this.denot.paramRef

    def toText(printer: Printer): Text = printer.toText(this)

    override def toString: String =
      Symbols.lastDenot(this).toString // + "#" + id // !!! DEBUG

    override def hashCode(): Int = Symbols.id(this) // for debugging.
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  extension (x: Any)
    inline def isSymbol: Boolean = x.isInstanceOf[Symbol]
    inline def asSymbol: Symbol = x.asInstanceOf[Symbol]

  extension (_self: Symbol)
    def self = _self.initialDenot

    private def lastDenot: SymDenotation = self.lastDenot
    private def lastDenot_=(d: SymDenotation): Unit = self.lastDenot = d

    /** A unique identifier of the symbol (unique per ContextBase) */
    def id: Int = self.common.id

    def nestingLevel: Int = self.common.nestingLevel

    /** The coordinates of the symbol (a position or an index) */
    def coord: Coord = lastDenot.common.coord

    /** Set the coordinate of this class, this is only useful when the coordinate is
     *  not known at symbol creation. This is the case for root symbols
     *  unpickled from TASTY.
     *
     *  @pre coord == NoCoord
     */
    private[core] def coord_=(c: Coord): Unit =
      //assert(myCoord == NoCoord)
        // This assertion fails for CommentPickling test.
        // TODO: figure out what's wrong in the setup of CommentPicklingTest and re-enable assertion.
      lastDenot.common.coord = c

    /** The tree defining the symbol at pickler time, EmptyTree if none was retained */
    def defTree: Tree =
      val dt = lastDenot.common.defTree
      if dt == null then EmptyTree else dt.nn

    /** Set defining tree if this symbol retains its definition tree */
    def defTree_=(tree: Tree)(using Context): Unit =
      if retainsDefTree then lastDenot.common.defTree = tree

    /** Does this symbol retain its definition tree?
     *  A good policy for this needs to balance costs and benefits, where
     *  costs are mainly memoty leaks, in particular across runs.
     */
    def retainsDefTree(using Context): Boolean =
      ctx.settings.YretainTrees.value ||
      denot.owner.isTerm ||                // no risk of leaking memory after a run for these
      denot.isOneOf(InlineOrProxy) ||      // need to keep inline info
      ctx.settings.YcheckInit.value        // initialization check

    /** Set the denotation of this symbol
     *  `denot` should always be initialized when a new Symbol is created.
     */
    private[core] def denot_=(d: SymDenotation): Unit =
      util.Stats.record("Symbol.denot_=")
      lastDenot = d
      self.checkedPeriod = Nowhere

    /** The current denotation of this symbol */
    def denot(using Context): SymDenotation =
      util.Stats.record("Symbol.denot")
      val lastd = lastDenot
      if self.checkedPeriod.code == ctx.period.code then lastd
      else computeDenot(lastd)

    def computeDenot(lastd: SymDenotation)(using Context): SymDenotation =
      util.Stats.record("Symbol.computeDenot")
      val now = ctx.period
      self.checkedPeriod = now
      if lastd.validFor contains now then lastd else recomputeDenot(lastd)

    private def recomputeDenot(lastd: SymDenotation)(using Context): SymDenotation =
      util.Stats.record("Symbol.recomputeDenot")
      val newd = lastd.currentSymDenot
      lastDenot = newd
      newd

    /** The original denotation of this symbol, without forcing anything */
    def originDenotation: SymDenotation = self

    /** The last known denotation of this symbol, without going through `current` */
    def lastKnownDenotation: SymDenotation = lastDenot

    def classDenot(using Context): ClassDenotation =
      _self.denot.asInstanceOf[ClassDenotation]

    private[core] def defRunId: RunId =
      lastDenot.validFor.runId

    /** Does this symbol come from a currently compiled source file? */
    def isDefinedInCurrentRun(using Context): Boolean =
      self.span.exists
      && defRunId == ctx.runId
      && lastDenot.associatedFileMatches(ctx.run.nn.files.contains)

    /** Is this symbol valid in the current run and has an associated file that is
      * not a binary file. e.g. This will return true for
      * symbols defined by the user in a prior run of the REPL, that are still valid.
      */
    def isDefinedInSource(using Context): Boolean =
      self.span.exists
      && isValidInCurrentRun
      && lastDenot.associatedFileMatches(_.extension != "class")

    def isValidInCurrentRun(using Context): Boolean =
      val d = lastDenot
      (d.validFor.runId == ctx.runId || stillValid(d))
      && (d.symbol eq _self)
        // the last condition is needed because under ctx.staleOK overwritten
        // members keep denotations pointing to the new symbol, so the validity
        // periods check out OK. But once a package member is overridden it is not longer
        // valid. If the option would be removed, the check would be no longer needed.

    def isTerm(using Context): Boolean =
      val lastd = lastDenot
      (if lastd.validFor.runId == ctx.runId then lastd else denot).isTerm

    def isType(using Context): Boolean =
      val lastd = lastDenot
      (if lastd.validFor.runId == ctx.runId then lastd else denot).isType

    def asTerm(using Context): TermSymbol =
      assert(self.isTerm, s"asTerm called on not-a-Term $this" )
      _self.asInstanceOf[TermSymbol]

    def asType(using Context): TypeSymbol =
      assert(self.isType, s"isType called on not-a-Type $this");
      _self.asInstanceOf[TypeSymbol]

    def isClass: Boolean = self.isClass

    def asClass: ClassSymbol = _self.asInstanceOf[ClassSymbol]

    /** Test whether symbol is private. This conservatively returns `false`
     *  if symbol's denotation is a class that is not yet read.
     */
    def isPrivate(using Context): Boolean =
      lastDenot.flagsUNSAFE.is(Private)

    /** Is the symbol a pattern bound symbol? */
    def isPatternBound(using Context): Boolean =
      !isClass && denot.is(Case, butNot = Enum | Module)

    /** The symbol's signature if it is completed or a method, NotAMethod otherwise. */
    def signature(using Context): Signature =
      if lastDenot.isCompleted || lastDenot.is(Method) then
        denot.signature
      else
        Signature.NotAMethod

    def isStatic(using Context): Boolean =
      self.isStatic

    /** This symbol entered into owner's scope (owner must be a class). */
    final def entered(using Context): _self.type =
      val d = denot
      if d.owner.isClass then
        d.owner.asClass.enter(_self)
        if d.is(Module) then d.owner.asClass.enter(d.moduleClass)
      _self


    /** Enter this symbol in its class owner after given `phase`. Create a fresh
     *  denotation for its owner class if the class does not already have one
     *  that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def enteredAfter(phase: DenotTransformer)(using Context): _self.type =
      if ctx.phaseId != phase.next.id then
        atPhase(phase.next)(enteredAfter(phase))
      else
        val d = denot
        d.owner match
          case owner: ClassSymbol =>
            if owner._is(Package) then
              d.validFor |= InitialPeriod
              if d.is(Module) then d.moduleClass.denot.validFor |= InitialPeriod
            else
              owner.classDenot.ensureFreshScopeAfter(phase)
            assert(isPrivate || phase.changesMembers, i"$_self entered in $owner at undeclared phase $phase")
            _self.entered
          case _ => _self

    /** Remove symbol from scope of owning class */
    final def drop()(using Context): Unit =
      val d = denot
      d.owner.classDenot.delete(_self)
      if d.is(Module) then d.owner.classDenot.delete(d.moduleClass)

    /** Remove symbol from scope of owning class after given `phase`. Create a fresh
     *  denotation for its owner class if the class does not already have one that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def dropAfter(phase: DenotTransformer)(using Context): Unit =
      if ctx.phaseId != phase.next.id then
        atPhase(phase.next)(dropAfter(phase))
      else
        val d = denot
        assert(!d.owner._is(Package))
        d.owner.classDenot.ensureFreshScopeAfter(phase)
        assert(isPrivate || phase.changesMembers, i"$_self deleted in ${d.owner} at undeclared phase $phase")
        drop()

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    inline def orElse(inline that: Symbol)(using Context): Symbol =
      if denot.exists then _self else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if p(_self) then _self else NoSymbol

    /** The current name of this symbol */
    def name(using Context): _self.ThisName = denot.name.asInstanceOf[_self.ThisName]

    /** The source or class file from which this class or
     *  the class containing this symbol was generated, null if not applicable.
     *  Note that this the returned classfile might be the top-level class
     *  containing this symbol instead of the directly enclosing class.
     *  Overridden in ClassSymbol
     */
    def associatedFile(using Context): AbstractFile | Null =
      lastDenot.associatedFile

    /** The class file from which this class was generated, null if not applicable. */
    final def binaryFile(using Context): AbstractFile | Null =
      val file = associatedFile
      if file != null && file.extension == "class" then file else null

   /** A trap to avoid calling x.symbol on something that is already a symbol.
     *  This would be expanded to `toDenot(x).symbol` which is guaraneteed to be
     *  the same as `x`.
     *  With the given setup, all such calls will produce `Nothing`
     */
    final def symbol: Nothing = unsupported("symbol")

    final def source(using Context): SourceFile =
      def valid(src: SourceFile): SourceFile =
        if src.exists && src.file.extension != "class" then src
        else NoSource

      val d = denot
      if !d.exists then NoSource
      else valid(defTree.source) match
        case NoSource =>
          valid(d.owner.source) match
            case NoSource =>
              _self match
                case cls: ClassSymbol  => valid(cls.sourceOfClass)
                case _ if d.is(Module) => valid(d.moduleClass.source)
                case _ => NoSource
            case src => src
        case src => src

    /** A symbol related to `sym` that is defined in source code.
     *
     *  @see enclosingSourceSymbols
     */
    @tailrec final def sourceSymbol(using Context): Symbol =
      val d = denot
      if !d.exists then
        _self
      else if d.is(ModuleVal) then
        d.moduleClass.sourceSymbol // The module val always has a zero-extent position
      else if d.is(Synthetic) then
        val linked = d.linkedClass
        if linked.exists && !linked._is(Synthetic) then linked
        else d.owner.sourceSymbol
      else if d.isPrimaryConstructor then
        d.owner.sourceSymbol
      else
        _self

    def exists(using Context): Boolean = _self.denot.exists
    def owner(using Context): Symbol = _self.denot.owner
    def typeParams(using Context): List[TypeSymbol] = _self.denot.typeParams
    def thisType(using Context): Type = _self.denot.thisType
    def typeRef(using Context): TypeRef = _self.denot.typeRef
    def termRef(using Context): TermRef = _self.denot.termRef
    def _info(using Context): Type = _self.denot.info
    def isCompleted(using Context): Boolean = _self.denot.isCompleted
    def isCompleting(using Context): Boolean = _self.denot.isCompleting
    def ensureCompleted()(using Context): Unit = _self.denot.ensureCompleted()
    def unforcedDecls(using Context): Scope = _self.denot.unforcedDecls
    def appliedRef(using Context): Type = _self.denot.appliedRef
    def namedType(using Context): NamedType = _self.denot.namedType
    def unforcedAnnotation(cls: Symbol)(using Context): Option[Annotation] = _self.denot.unforcedAnnotation(cls)
    def children(using Context): List[Symbol] = _self.denot.children
    def topLevelClass(using Context): Symbol = _self.denot.topLevelClass
    def moduleClass(using Context): Symbol = _self.denot.moduleClass
    def sourceModule(using Context): Symbol = _self.denot.sourceModule
    def underlyingSymbol(using Context): Symbol = _self.denot.underlyingSymbol
    def ownersIterator(using Context): Iterator[Symbol] = _self.denot.ownersIterator
    def enclosingClass(using Context): Symbol = _self.denot.enclosingClass
    def enclosingMethod(using Context): Symbol = _self.denot.enclosingMethod
    def typeParamCreationFlags(using Context): FlagSet = _self.denot.typeParamCreationFlags
    def _is(flag: Flag)(using Context): Boolean = _self.denot.is(flag)
    def _is(flag: Flag, butNot: FlagSet)(using Context): Boolean = _self.denot.is(flag, butNot)
    def _isOneOf(fs: FlagSet)(using Context): Boolean = _self.denot.isOneOf(fs)
    def _isOneOf(fs: FlagSet, butNot: FlagSet)(using Context): Boolean = _self.denot.isOneOf(fs, butNot)
    def _isAllOf(fs: FlagSet)(using Context): Boolean = _self.denot.isAllOf(fs)
    def _isAllOf(fs: FlagSet, butNot: FlagSet)(using Context): Boolean = _self.denot.isAllOf(fs, butNot)
    // !!! Dotty problem: overloaded extension methods here lead to failures like
    // Assertion failed: data race? overwriting method isAllOf with method isAllOf in type TermRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix,module class dotc)),object core),object Symbols),isAllOf),
    // |last sym id = 10301, new sym id = 10299,
    // |last owner = module class Symbols$, new owner = module class Symbols$,

    // SrcPos types and methods // !!! make them use lastDenot?
    def span: Span = self.span
    def sourcePos(using Context): SourcePosition = self.sourcePos
    def srcPos: SrcPos = self.srcPos

    // ParamInfo types and methods
    def isTypeParam(using Context): Boolean = denot.is(TypeParam)
    def paramName(using Context): _self.ThisName = denot.paramName.asInstanceOf
    def paramInfo(using Context): Type = denot.info
    def paramInfoAsSeenFrom(pre: Type)(using Context): Type = denot.paramInfoAsSeenFrom(pre)
    def paramInfoOrCompleter(using Context): Type = denot.paramInfoOrCompleter
    def paramVariance(using Context): Variance = denot.paramVariance
    def paramRef(using Context): TypeRef = denot.paramRef

    /** Copy a symbol, overriding selective fields.
     *  Note that `coord` and `associatedFile` will be set from the fields in `owner`, not
     *  the fields in `sym`.
     */
    def copy(using Context)(
        owner: Symbol = _self.denot.owner,
        name: _self.ThisName = _self.denot.name.asInstanceOf[_self.ThisName],
        flags: FlagSet = _self.denot.flags,
        info: Type = _self.denot.info,
        privateWithin: Symbol = _self.denot.privateWithin,
        coord: Coord = NoCoord, // Can be `= owner.coord` once we bootstrap
        associatedFile: AbstractFile | Null = null // Can be `= owner.associatedFile` once we bootstrap
    ): Symbol =
      val coord1 = if coord == NoCoord then owner.coord else coord
      val associatedFile1 = if associatedFile == null then owner.associatedFile else associatedFile

      if isClass then
        newClassSymbol(owner, name.asTypeName, flags, _ => info, privateWithin, coord1, associatedFile1)
      else
        newSymbol(owner, name, flags, info, privateWithin, coord1)

    // -------- Printing --------------------------------------------------------

    def toText(printer: Printer): Text = printer.toText(_self)

    def showLocated(using Context): String = ctx.printer.locatedText(_self).show
    def showExtendedLocation(using Context): String = ctx.printer.extendedLocationText(_self).show
    def showDcl(using Context): String = ctx.printer.dclText(_self).show
    def showKind(using Context): String = ctx.printer.kindString(_self)
    def showName(using Context): String = ctx.printer.nameString(_self)
    def showFullName(using Context): String = ctx.printer.fullNameString(_self)

    def debugString: String = self.debugString

  end extension

  type TreeOrProvider = TreeProvider | Tree

  class ClassSymbol private[Symbols] extends Symbol {

    util.Stats.record("ClassSymbol")

    type ThisName = TypeName
  }

  extension (_self: ClassSymbol)
    def self = _self.initialDenot

    /** If this is a top-level class and `-Yretain-trees` (or `-from-tasty`) is set.
      * Returns the TypeDef tree (possibly wrapped inside PackageDefs) for this class, otherwise EmptyTree.
      * This will force the info of the class.
      */
    def rootTree(using Context): Tree = rootTreeContaining("")

    /** Same as `rootTree` but load tree only if `id == ""` or the tree might contain `id`.
     *  For Tasty trees this means consulting whether the name table defines `id`.
     *  For already loaded trees, we maintain the referenced ids in an attachment.
     */
    def rootTreeContaining(id: String)(using Context): Tree =
      _self.denot.infoOrCompleter match
        case _: NoCompleter =>
        case _ => _self.denot.ensureCompleted()
      rootTreeOrProvider match
        case fn: TreeProvider =>
          if id.isEmpty || fn.mightContain(id) then
            val tree = fn.tree
            rootTreeOrProvider = tree
            tree
          else EmptyTree
        case tree: Tree @ unchecked =>
          if id.isEmpty || mightContain(tree, id) then tree else EmptyTree

    def rootTreeOrProvider: TreeOrProvider =
      _self.lastKnownDenotation.common.asClass.treeOrProvider

    private[dotc] def rootTreeOrProvider_=(t: TreeOrProvider)(using Context): Unit =
      _self.lastKnownDenotation.common.asClass.treeOrProvider = t

    private def mightContain(tree: Tree, id: String)(using Context): Boolean =
      val ids = tree.getAttachment(Ids) match
        case Some(ids) => ids
        case None =>
          val idSet = mutable.SortedSet[String]()
          tree.foreachSubTree {
            case tree: tpd.NameTree if tree.name.toTermName.isInstanceOf[SimpleName] =>
              idSet += tree.name.toString
            case _ =>
          }
          val ids = idSet.toArray
          tree.putAttachment(Ids, ids)
          ids
      ids.binarySearch(id) >= 0

    def assocFile: AbstractFile | Null = self.common.asClass.assocFile

    def sourceOfClass(using Context): SourceFile =
      val common = _self.lastKnownDenotation.common.asClass
      if !common.source.exists && !_self._is(Package) then
        // this allows sources to be added in annotations after `sourceOfClass` is first called
        val file = _self.associatedFile
        if file != null && file.extension != "class" then
          common.source = ctx.getSource(file)
        else
          common.source = defn.patchSource(_self)
          if !common.source.exists then
            common.source = atPhaseNoLater(flattenPhase) {
              _self.denot.topLevelClass.unforcedAnnotation(defn.SourceFileAnnot) match
                case Some(sourceAnnot) => sourceAnnot.argumentConstant(0) match
                  case Some(Constant(path: String)) => ctx.getSource(path)
                  case none => NoSource
                case none => NoSource
            }
      common.source

    private def enter(sym: Symbol, scope: Scope = EmptyScope)(using Context): Unit =
      _self.classDenot.enter(sym, scope)

    def classInfo(using Context): ClassInfo = _self.classDenot.classInfo

  end extension

  @sharable object NoSymbol extends Symbol {
    //override def coord = NoCoord
    //override def id = 0
    //override def nestingLevel = 0
    //override def defTree = tpd.EmptyTree
    //override def associatedFile(using Context): AbstractFile | Null = NoSource.file
    //override def recomputeDenot(lastd: SymDenotation)(using Context): SymDenotation = NoDenotation
  }

  NoDenotation // force it in order to set `denot` field of NoSymbol

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(using Context): SymDenotation = sym.denot

  /** Makes all class denotation operations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(using Context): ClassDenotation = cls.classDenot

  /** Blocks use of `toDenot` conversion in Symbols itself */
  private implicit def toDenotALT(sym: Symbol)(using Context): SymDenotation = sym.denot

  /** Blocks use of `toClassDenot` conversion in Symbols itself */
  private implicit def toClassDenotALT(cls: ClassSymbol)(using Context): ClassDenotation = cls.classDenot

  /** The Definitions object */
  def defn(using Context): Definitions = ctx.definitions

  /** The current class */
  def currentClass(using Context): ClassSymbol = ctx.owner.enclosingClass.asClass

  type MutableSymbolMap[T] = EqHashMap[Symbol, T]
  def MutableSymbolMap[T](): EqHashMap[Symbol, T] = EqHashMap[Symbol, T]()
  def MutableSymbolMap[T](initialCapacity: Int): EqHashMap[Symbol, T] = EqHashMap[Symbol, T](initialCapacity)

// ---- Symbol creation methods ----------------------------------

  /** Create a symbol from its fields (info may be lazy) */
  def newSymbol[N <: Name](using Context)(
      owner: Symbol,
      name: N,
      flags: FlagSet,
      info: Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      nestingLevel: Int = ctx.nestingLevel): Symbol { type ThisName = N } = {
    val sym = new Symbol().asInstanceOf[Symbol { type ThisName = N }]
    val denot = SymDenotation(sym, SymCommon(coord, ctx.base.nextSymId, nestingLevel), owner, name, flags, info, privateWithin)
    sym.initialDenot = denot
    sym.denot_=(denot)
    sym
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
      assocFile: AbstractFile | Null = null)(using Context): ClassSymbol
  = {
    val cls = new ClassSymbol()
    val denot = SymDenotation(cls, ClassCommon(coord, ctx.base.nextSymId, ctx.nestingLevel, assocFile), owner, name, flags, NoType, privateWithin)
    cls.initialDenot = denot
    cls.denot_=(denot)
    denot.info = infoFn(cls)
    cls
  }

  /** Create a class symbol from its non-info fields and the fields of its info. */
  def newCompleteClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile | Null = null)(using Context): ClassSymbol =
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
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile | Null = null)(using Context): ClassSymbol = {
    def completer = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        denot.info = ClassInfo(owner.thisType, cls, parentTypes.map(_.dealias), decls, selfInfo)
      }
    }
    newClassSymbol(owner, name, flags, completer, privateWithin, coord, assocFile)
  }

  def newRefinedClassSymbol(coord: Coord = NoCoord)(using Context): ClassSymbol =
    newCompleteClassSymbol(ctx.owner, tpnme.REFINE_CLASS, NonMember, parents = Nil, newScope, coord = coord)

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
      assocFile: AbstractFile | Null = null)(using Context): TermSymbol
  = {
    val base = owner.thisType
    val modclsFlags = clsFlags | ModuleClassCreationFlags
    val modclsName = name.toTypeName.adjustIfModuleClass(modclsFlags)
    val module = newSymbol(
      owner, name, modFlags | ModuleValCreationFlags, NoCompleter, privateWithin, coord)
    val modcls = newClassSymbol(
      owner, modclsName, modclsFlags, infoFn(module, _), privateWithin, coord, assocFile)
    module.denot.info =
      if (modcls.isCompleted) TypeRef(owner.thisType, modcls)
      else new ModuleCompleter(modcls)
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
      assocFile: AbstractFile | Null = null)(using Context): TermSymbol =
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => ClassInfo(
          owner.thisType, modcls, parents, decls, TermRef(owner.thisType, module)),
        privateWithin, coord, assocFile)

  /** Same as `newCompleteModuleSymbol` except that `parents` can be a list of arbitrary
   *  types which get normalized into type refs and parameter bindings.
   */
  def newNormalizedModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      parentTypes: List[Type],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile | Null = null)(using Context): TermSymbol = {
    def completer(module: Symbol) = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        denot.info = ClassInfo(owner.thisType, cls, parentTypes.map(_.dealias), decls, TermRef(owner.thisType, module))
      }
    }
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => completer(module),
        privateWithin, coord, assocFile)
  }

  /** Create a package symbol with associated package class
   *  from its non-info fields and a lazy type for loading the package's members.
   */
  def newPackageSymbol(
      owner: Symbol,
      name: TermName,
      infoFn: (TermSymbol, ClassSymbol) => LazyType)(using Context): TermSymbol =
    newModuleSymbol(owner, name, PackageCreationFlags, PackageCreationFlags, infoFn)
  /** Create a package symbol with associated package class
   *  from its non-info fields its member scope.
   */
  def newCompletePackageSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet = EmptyFlags,
      clsFlags: FlagSet = EmptyFlags,
      decls: Scope = newScope(0))(using Context): TermSymbol =
    newCompleteModuleSymbol(
      owner, name,
      modFlags | PackageCreationFlags, clsFlags | PackageCreationFlags,
      Nil, decls)

  /** Define a new symbol associated with a Bind or pattern wildcard and, by default, make it gadt narrowable. */
  def newPatternBoundSymbol(
      name: Name,
      info: Type,
      span: Span,
      addToGadt: Boolean = true,
      flags: FlagSet = EmptyFlags)(using Context): Symbol = {
    val sym = newSymbol(ctx.owner, name, Case | flags, info, coord = span)
    if (addToGadt && name.isTypeName) ctx.gadtState.addToConstraint(sym)
    sym
  }

  /** Create a stub symbol that will issue a missing reference error
   *  when attempted to be completed.
   */
  def newStubSymbol(owner: Symbol, name: Name, file: AbstractFile | Null = null)(using Context): Symbol = {
    def stubCompleter = new StubInfo()
    val normalizedOwner = if (owner._is(ModuleVal)) owner.moduleClass else owner
    typr.println(s"creating stub for ${name.show}, owner = ${normalizedOwner.denot.debugString}, file = $file")
    typr.println(s"decls = ${normalizedOwner.unforcedDecls.toList.map(_.debugString).mkString("\n  ")}") // !!! DEBUG
    //if (base.settings.debug.value) throw new Error()
    val stub = name match {
      case name: TermName =>
        newModuleSymbol(normalizedOwner, name, EmptyFlags, EmptyFlags, stubCompleter, assocFile = file)
      case name: TypeName =>
        newClassSymbol(normalizedOwner, name, EmptyFlags, stubCompleter, assocFile = file)
    }
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
  def newLocalDummy(cls: Symbol, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(cls, nme.localDummyName(cls), NonMember, NoType)

  /** Create an import symbol pointing back to given qualifier `expr`. */
  def newImportSymbol(owner: Symbol, expr: Tree, coord: Coord = NoCoord)(using Context): TermSymbol =
    newImportSymbol(owner, ImportType(expr), coord = coord)

  /** Create an import symbol with given `info`. */
  def newImportSymbol(owner: Symbol, info: Type, coord: Coord)(using Context): TermSymbol =
    newSymbol(owner, nme.IMPORT, Synthetic | NonMember, info, coord = coord)

  /** Create a class constructor symbol for given class `cls`. */
  def newConstructor(
      cls: ClassSymbol,
      flags: FlagSet,
      paramNames: List[TermName],
      paramTypes: List[Type],
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, MethodType(paramNames, paramTypes, cls.typeRef), privateWithin, coord)

  /** Create an anonymous function symbol */
  def newAnonFun(owner: Symbol, info: Type, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(owner, nme.ANON_FUN, Synthetic | Method, info, coord = coord)

  /** Create an empty default constructor symbol for given class `cls`. */
  def newDefaultConstructor(cls: ClassSymbol)(using Context): TermSymbol =
    newConstructor(cls, EmptyFlags, Nil, Nil)

  def newLazyImplicit(info: Type, coord: Coord = NoCoord)(using Context): TermSymbol =
    newSymbol(ctx.owner, LazyImplicitName.fresh(), EmptyFlags, info, coord = coord)

  /** Create a symbol representing a selftype declaration for class `cls`. */
  def newSelfSym(
      cls: ClassSymbol,
      name: TermName = nme.WILDCARD,
      selfInfo: Type = NoType)(using Context): TermSymbol =
    newSymbol(cls, name, SelfSymFlags, selfInfo orElse cls.classInfo.selfType, coord = cls.coord)

  /** Create new type parameters with given owner, names, and flags.
   *  @param boundsFn  A function that, given type refs to the newly created
   *                   parameters returns a list of their bounds.
   */
  def newTypeParams(
    owner: Symbol,
    names: List[TypeName],
    flags: FlagSet,
    boundsFn: List[TypeRef] => List[Type])(using Context): List[TypeSymbol] = {

    val tparamBuf = new mutable.ListBuffer[TypeSymbol]
    val trefBuf = new mutable.ListBuffer[TypeRef]
    for (name <- names) {
      val tparam = newSymbol(
        owner, name, flags | owner.typeParamCreationFlags, NoType, coord = owner.coord)
      tparamBuf += tparam
      trefBuf += TypeRef(owner.thisType, tparam)
    }
    val tparams = tparamBuf.toList
    val bounds = boundsFn(trefBuf.toList)
    for (tparam, bound) <- tparams.lazyZip(bounds) do
      tparam.denot.info = bound
    tparams
  }

  /** Create a new skolem symbol. This is not the same as SkolemType, even though the
   *  motivation (create a singleton referencing to a type) is similar.
   */
  def newSkolem(tp: Type)(using Context): TermSymbol =
    newSymbol(defn.RootClass, nme.SKOLEM, SyntheticArtifact | NonMember | Permanent, tp)

  def newErrorSymbol(owner: Symbol, name: Name, msg: Message)(using Context): Symbol = {
    val errType = ErrorType(msg)
    newSymbol(owner, name, SyntheticArtifact,
        if (name.isTypeName) TypeAlias(errType) else errType)
  }

  /** Map given symbols, subjecting their attributes to the mappings
   *  defined in the given TreeTypeMap `ttmap`.
   *  Cross symbol references are brought over from originals to copies.
   *  Do not copy any symbols if all attributes of all symbols stay the same.
   */
  def mapSymbols(originals: List[Symbol], ttmap: TreeTypeMap, mapAlways: Boolean = false)(using Context): List[Symbol] =
    if (originals.forall(sym =>
        (ttmap.mapType(sym._info) eq sym._info) &&
        !(ttmap.oldOwners contains sym.owner)) && !mapAlways)
      originals
    else {
      val copies: List[Symbol] = for (original <- originals) yield
        val odenot = original.denot
        original.copy(
          owner = ttmap.mapOwner(odenot.owner),
          flags = odenot.flags &~ Touched,
          info = NoCompleter,
          privateWithin = ttmap.mapOwner(odenot.privateWithin),
          coord = original.coord)
      val ttmap1 = ttmap.withSubstitution(originals, copies)
      originals.lazyZip(copies) foreach { (original, copy) =>
        val odenot = original.denot
        val completer = new LazyType:

          def complete(denot: SymDenotation)(using Context): Unit =

            val oinfo = original._info match
              case ClassInfo(pre, _, parents, decls, selfInfo) =>
                assert(original.isClass)
                val parents1 = parents.mapConserve(ttmap.mapType)
                val otypeParams = original.typeParams
                if otypeParams.isEmpty then
                  ClassInfo(pre, copy.asClass, parents1, decls.cloneScope, selfInfo)
                else
                  // copy type params, enter other definitions unchanged
                  // type parameters need to be copied early, since other type
                  // computations depend on them.
                  val decls1 = newScope
                  val newTypeParams = mapSymbols(original.typeParams, ttmap1, mapAlways = true)
                  newTypeParams.foreach(decls1.enter)
                  for sym <- decls do if !sym._is(TypeParam) then decls1.enter(sym)
                  val parents2 = parents1.map(_.substSym(otypeParams, newTypeParams))
                  val selfInfo1 = selfInfo match
                    case selfInfo: Type => selfInfo.substSym(otypeParams, newTypeParams)
                    case _ => selfInfo
                  ClassInfo(pre, copy.asClass, parents2, decls1, selfInfo1)
              case oinfo => oinfo

            denot.info = oinfo // needed as otherwise we won't be able to go from Sym -> parents & etc
                               // Note that this is a hack, but hack commonly used in Dotty
                               // The same thing is done by other completers all the time
            denot.info = ttmap1.mapType(oinfo)
            denot.annotations = odenot.annotations.mapConserve(ttmap1.apply)

        end completer

        copy.denot.info = completer
        copy.denot match
          case cd: ClassDenotation =>
            cd.registeredCompanion = original.denot.registeredCompanion.subst(originals, copies)
          case _ =>
      }

      copies.foreach(_.ensureCompleted()) // avoid memory leak

      // Update Child annotations of classes encountered previously to new values
      // if some child is among the mapped symbols
      for orig <- ttmap1.substFrom do
        if orig._is(Sealed) && orig.children.exists(originals.contains) then
          val sealedCopy = orig.subst(ttmap1.substFrom, ttmap1.substTo)
          sealedCopy.denot.annotations = sealedCopy.denot.annotations.mapConserve(ttmap1.apply)

      copies
    }

  /** Matches lists of term symbols, including the empty list.
   *  All symbols in the list are assumed to be of the same kind.
   */
  object TermSymbols:
    def unapply(xs: List[Symbol])(using Context): Option[List[TermSymbol]] = xs match
      case (x: Symbol) :: _ if x.isType => None
      case _ => Some(xs.asInstanceOf[List[TermSymbol]])

  /** Matches lists of type symbols, excluding the empty list.
   *  All symbols in the list are assumed to be of the same kind.
   */
  object TypeSymbols:
    def unapply(xs: List[Symbol])(using Context): Option[List[TypeSymbol]] = xs match
      case (x: Symbol) :: _ if x.isType => Some(xs.asInstanceOf[List[TypeSymbol]])
      case _ => None

// ----- Locating predefined symbols ----------------------------------------

  def requiredPackage(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name, isPackage = true).requiredSymbol("package", name)(_._is(Package)).asTerm
  }

  def requiredPackageRef(path: PreName)(using Context): TermRef = requiredPackage(path).termRef

  def requiredClass(path: PreName)(using Context): ClassSymbol = {
    val name = path.toTypeName
    staticRef(name).requiredSymbol("class", name)(_.isClass) match {
      case cls: ClassSymbol => cls
      case sym => defn.AnyClass
    }
  }

  def requiredClassRef(path: PreName)(using Context): TypeRef = requiredClass(path).typeRef

  /** Get ClassSymbol if class is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getClassIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTypeName, generateStubs = false)
      .disambiguate(_.isClass).symbol

  /** Get a List of ClassSymbols which are either defined in current compilation
   *  run or present on classpath.
   */
  def getClassesIfDefined(paths: List[PreName])(using Context): List[ClassSymbol] =
    paths.map(getClassIfDefined).filter(_.exists).map(_.asInstanceOf[ClassSymbol])

  /** Get ClassSymbol if package is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getPackageClassIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTypeName, isPackage = true, generateStubs = false)
      .disambiguate(_._is(PackageClass)).symbol

  def requiredModule(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("object", name)(_._is(Module)).asTerm
  }

  /** Get module symbol if the module is either defined in current compilation run
   *  or present on classpath. Returns NoSymbol otherwise.
   */
  def getModuleIfDefined(path: PreName)(using Context): Symbol =
    staticRef(path.toTermName, generateStubs = false)
      .disambiguate(_._is(Module)).symbol

  def requiredModuleRef(path: PreName)(using Context): TermRef = requiredModule(path).termRef

  def requiredMethod(path: PreName)(using Context): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("method", name)(_._is(Method)).asTerm
  }

  def requiredMethodRef(path: PreName)(using Context): TermRef = requiredMethod(path).termRef
}
