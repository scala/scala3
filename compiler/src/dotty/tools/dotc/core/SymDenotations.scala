package dotty.tools
package dotc
package core

import Periods._, Contexts._, Symbols._, Denotations._, Names._, NameOps._, Annotations._
import Types._, Flags._, Decorators._, DenotTransformers._, StdNames._, Scopes._
import NameOps._, NameKinds._, Phases._
import Constants.Constant
import TypeApplications.TypeParamInfo
import Scopes.Scope
import dotty.tools.io.AbstractFile
import Decorators.SymbolIteratorDecorator
import ast._
import Trees.Literal
import annotation.tailrec
import util.SimpleIdentityMap
import util.Stats
import java.util.WeakHashMap
import config.Config
import reporting.diagnostic.Message
import reporting.diagnostic.messages.BadSymbolicReference
import reporting.trace
import collection.mutable
import transform.TypeUtils._

import scala.annotation.internal.sharable

trait SymDenotations { this: Context =>
  import SymDenotations._

  /** Factory method for SymDenotion creation. All creations
   *  should be done via this method.
   */
  def SymDenotation(
    symbol: Symbol,
    owner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol)(implicit ctx: Context): SymDenotation = {
    val result =
      if (symbol.isClass)
        if (initFlags is Package) new PackageClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
        else new ClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
      else new SymDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
    result.validFor = stablePeriod
    result
  }

  def stillValid(denot: SymDenotation): Boolean =
    if (denot.is(ValidForever) || denot.isRefinementClass || denot.isImport) true
    else {
      val initial = denot.initial
      val firstPhaseId = initial.validFor.firstPhaseId.max(ctx.typerPhase.id)
      if ((initial ne denot) || ctx.phaseId != firstPhaseId)
        ctx.withPhase(firstPhaseId).stillValidInOwner(initial)
      else
        stillValidInOwner(denot)
    }

  private[SymDenotations] def stillValidInOwner(denot: SymDenotation): Boolean = try {
    val owner = denot.owner.denot
    stillValid(owner) && (
      !owner.isClass
      || owner.isRefinementClass
      || owner.is(Scala2x)
      || (owner.unforcedDecls.lookupAll(denot.name) contains denot.symbol)
      || denot.isSelfSym
      || denot.isLocalDummy)
  } catch {
    case ex: StaleSymbol => false
  }

  /** Explain why symbol is invalid; used for debugging only */
  def traceInvalid(denot: Denotation): Boolean = {
    def show(d: Denotation) = s"$d#${d.symbol.id}"
    def explain(msg: String) = {
      println(s"${show(denot)} is invalid at ${this.period} because $msg")
      false
    }
    denot match {
      case denot: SymDenotation =>
        def explainSym(msg: String) = explain(s"$msg\ndefined = ${denot.definedPeriodsString}")
        if (denot.is(ValidForever) || denot.isRefinementClass) true
        else {
          implicit val ctx = this
          val initial = denot.initial
          if ((initial ne denot) || ctx.phaseId != initial.validFor.firstPhaseId) {
            ctx.withPhase(initial.validFor.firstPhaseId).traceInvalid(initial)
          } else try {
            val owner = denot.owner.denot
            if (!traceInvalid(owner)) explainSym("owner is invalid")
            else if (!owner.isClass || owner.isRefinementClass || denot.isSelfSym) true
            else if (owner.unforcedDecls.lookupAll(denot.name) contains denot.symbol) true
            else explainSym(s"decls of ${show(owner)} are ${owner.unforcedDecls.lookupAll(denot.name).toList}, do not contain ${denot.symbol}")
          } catch {
            case ex: StaleSymbol => explainSym(s"$ex was thrown")
          }
      }
      case _ =>
        explain("denotation is not a SymDenotation")
    }
  }

  /** Configurable: Accept stale symbol with warning if in IDE */
  def staleOK: Boolean = Config.ignoreStaleInIDE && mode.is(Mode.Interactive)

  /** Possibly accept stale symbol with warning if in IDE */
  def acceptStale(denot: SingleDenotation): Boolean =
    staleOK && {
      ctx.echo(denot.staleSymbolMsg)
      true
    }
}

object SymDenotations {

  /** A sym-denotation represents the contents of a definition
   *  during a period.
   */
  class SymDenotation private[SymDenotations] (
    symbol: Symbol,
    final val maybeOwner: Symbol,
    final val name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol, initInfo) {

    //assert(symbol.id != 4940, name)

    override def hasUniqueSym: Boolean = exists

    /** Debug only
    override def validFor_=(p: Period) = {
      super.validFor_=(p)
    }
    */
    if (Config.checkNoSkolemsInInfo) assertNoSkolems(initInfo)

    // ------ Getting and setting fields -----------------------------

    private[this] var myFlags: FlagSet = adaptFlags(initFlags)
    private[this] var myPrivateWithin: Symbol = initPrivateWithin
    private[this] var myAnnotations: List[Annotation] = Nil

    /** The owner of the symbol; overridden in NoDenotation */
    def owner: Symbol = maybeOwner

    /** The flag set */
    final def flags(implicit ctx: Context): FlagSet = { ensureCompleted(); myFlags }

    /** The flag set without forcing symbol completion.
     *  Should be used only for printing.
     */
    private[dotc] final def flagsUNSAFE: FlagSet = myFlags

    /** Adapt flag set to this denotation's term or type nature */
    private def adaptFlags(flags: FlagSet) = if (isType) flags.toTypeFlags else flags.toTermFlags

    /** Update the flag set */
    final def flags_=(flags: FlagSet): Unit =
      myFlags = adaptFlags(flags)

    /** Set given flags(s) of this denotation */
    final def setFlag(flags: FlagSet): Unit = { myFlags |= flags }

    /** Unset given flags(s) of this denotation */
    final def resetFlag(flags: FlagSet): Unit = { myFlags &~= flags }

    /** Set applicable flags in {NoInits, PureInterface}
     *  @param  parentFlags  The flags that match the class or trait's parents
     *  @param  bodyFlags    The flags that match the class or trait's body
     */
    final def setNoInitsFlags(parentFlags: FlagSet, bodyFlags: FlagSet): Unit =
      setFlag(
        if (myFlags.is(Trait)) NoInitsInterface & bodyFlags // no parents are initialized from a trait
        else NoInits & bodyFlags & parentFlags)

    private def isCurrent(fs: FlagSet) =
      fs <= (
        if (myInfo.isInstanceOf[SymbolLoader]) FromStartFlags
        else AfterLoadFlags)

    final def relevantFlagsFor(fs: FlagSet)(implicit ctx: Context) =
      if (isCurrent(fs)) myFlags else flags

    /** Has this denotation one of the flags in `fs` set? */
    final def is(fs: FlagSet)(implicit ctx: Context): Boolean =
      (if (isCurrent(fs)) myFlags else flags) is fs

    /** Has this denotation one of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def is(fs: FlagSet, butNot: FlagSet)(implicit ctx: Context): Boolean =
      (if (isCurrent(fs) && isCurrent(butNot)) myFlags else flags) is (fs, butNot)

    /** Has this denotation all of the flags in `fs` set? */
    final def is(fs: FlagConjunction)(implicit ctx: Context): Boolean =
      (if (isCurrent(fs)) myFlags else flags) is fs

    /** Has this denotation all of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def is(fs: FlagConjunction, butNot: FlagSet)(implicit ctx: Context): Boolean =
      (if (isCurrent(fs) && isCurrent(butNot)) myFlags else flags) is (fs, butNot)

    /** The type info, or, if symbol is not yet completed, the completer */
    final def infoOrCompleter: Type = myInfo

    /** Optionally, the info if it is completed */
    final def unforcedInfo: Option[Type] = myInfo match {
      case myInfo: LazyType => None
      case _ => Some(myInfo)
    }

    final def completeFrom(completer: LazyType)(implicit ctx: Context): Unit =
      if (Config.showCompletions) {
        println(i"${"  " * indent}completing ${if (isType) "type" else "val"} $name")
        indent += 1

        if (myFlags is Touched) throw CyclicReference(this)
        myFlags |= Touched

        // completions.println(s"completing ${this.debugString}")
        try completer.complete(this)(ctx.withPhase(validFor.firstPhaseId))
        catch {
          case ex: CyclicReference =>
            println(s"error while completing ${this.debugString}")
            throw ex
        }
        finally {
          indent -= 1
          println(i"${"  " * indent}completed $name in $owner")
        }
      }
      else {
        if (myFlags is Touched) throw CyclicReference(this)
        myFlags |= Touched
        completer.complete(this)(ctx.withPhase(validFor.firstPhaseId))
      }

    protected[dotc] def info_=(tp: Type): Unit = {
      /* // DEBUG
       def illegal: String = s"illegal type for $this: $tp"
      if (this is Module) // make sure module invariants that allow moduleClass and sourceModule to work are kept.
        tp match {
          case tp: ClassInfo => assert(tp.selfInfo.isInstanceOf[TermRefBySym], illegal)
          case tp: NamedType => assert(tp.isInstanceOf[TypeRefBySym], illegal)
          case tp: ExprType => assert(tp.resultType.isInstanceOf[TypeRefBySym], illegal)
          case _ =>
        }
        */
      if (Config.checkNoSkolemsInInfo) assertNoSkolems(tp)
      myInfo = tp
    }

    /** The name, except
     *   - if this is a module class, strip the module class suffix
     *   - if this is a companion object with a clash-avoiding name, strip the
     *     "avoid clash" suffix
     */
    def effectiveName(implicit ctx: Context): Name =
      if (this is ModuleClass) name.stripModuleClassSuffix
      else name.exclude(AvoidClashName)

    /** The privateWithin boundary, NoSymbol if no boundary is given.
     */
    final def privateWithin(implicit ctx: Context): Symbol = { ensureCompleted(); myPrivateWithin }

    /** Set privateWithin. */
    protected[dotc] final def privateWithin_=(sym: Symbol): Unit =
      myPrivateWithin = sym

    /** The annotations of this denotation */
    final def annotations(implicit ctx: Context): List[Annotation] = {
      ensureCompleted(); myAnnotations
    }

    /** Update the annotations of this denotation */
    final def annotations_=(annots: List[Annotation]): Unit =
      myAnnotations = annots

    /** Does this denotation have an annotation matching the given class symbol? */
    final def hasAnnotation(cls: Symbol)(implicit ctx: Context): Boolean =
      dropOtherAnnotations(annotations, cls).nonEmpty

    /** Apply transform `f` to all annotations of this denotation */
    final def transformAnnotations(f: Annotation => Annotation)(implicit ctx: Context): Unit =
      annotations = annotations.mapConserve(f)

    /** Keep only those annotations that satisfy `p` */
    final def filterAnnotations(p: Annotation => Boolean)(implicit ctx: Context): Unit =
      annotations = annotations.filterConserve(p)

    /** Optionally, the annotation matching the given class symbol */
    final def getAnnotation(cls: Symbol)(implicit ctx: Context): Option[Annotation] =
      dropOtherAnnotations(annotations, cls) match {
        case annot :: _ => Some(annot)
        case nil => None
      }

    /** The same as getAnnotation, but without ensuring
     *  that the symbol carrying the annotation is completed
     */
    final def unforcedAnnotation(cls: Symbol)(implicit ctx: Context): Option[Annotation] =
      dropOtherAnnotations(myAnnotations, cls) match {
        case annot :: _ => Some(annot)
        case nil => None
      }

    /** Add given annotation to the annotations of this denotation */
    final def addAnnotation(annot: Annotation): Unit =
      annotations = annot :: myAnnotations

    /** Remove annotation with given class from this denotation */
    final def removeAnnotation(cls: Symbol)(implicit ctx: Context): Unit =
      annotations = myAnnotations.filterNot(_ matches cls)

    /** Remove any annotations with same class as `annot`, and add `annot` */
    final def updateAnnotation(annot: Annotation)(implicit ctx: Context): Unit = {
      removeAnnotation(annot.symbol)
      addAnnotation(annot)
    }

    /** Add all given annotations to this symbol */
    final def addAnnotations(annots: TraversableOnce[Annotation])(implicit ctx: Context): Unit =
      annots.foreach(addAnnotation)

    @tailrec
    private def dropOtherAnnotations(anns: List[Annotation], cls: Symbol)(implicit ctx: Context): List[Annotation] = anns match {
      case ann :: rest => if (ann matches cls) anns else dropOtherAnnotations(rest, cls)
      case Nil => Nil
    }

    /** The denotation is completed: info is not a lazy type and attributes have defined values */
    final def isCompleted: Boolean = !myInfo.isInstanceOf[LazyType]

    /** The denotation is in train of being completed */
    final def isCompleting: Boolean = (myFlags is Touched) && !isCompleted

    /** The completer of this denotation. @pre: Denotation is not yet completed */
    final def completer: LazyType = myInfo.asInstanceOf[LazyType]

    /** Make sure this denotation is completed */
    final def ensureCompleted()(implicit ctx: Context): Unit = info

    /** The symbols defined in this class or object.
     *  Careful! This does not force the type, so is compilation order dependent.
     *  This method should be used only in the following circumstances:
     *
     *  1. When accessing type parameters or type parameter accessors (both are entered before
     *     completion).
     *  2. When obtaining the current scope in order to enter, rename or delete something there.
     *  3. When playing it safe in order not to raise CylicReferences, e.g. for printing things
     *     or taking more efficient shortcuts (e.g. the stillValid test).
     */
    final def unforcedDecls(implicit ctx: Context): Scope = myInfo match {
      case cinfo: LazyType =>
        val knownDecls = cinfo.decls
        if (knownDecls ne EmptyScope) knownDecls
        else { completeFrom(cinfo); unforcedDecls } // complete-once
      case _ => info.decls
    }

    /** If this is a package class, the symbols entered in it
     *  before it is completed. (this is needed to eagerly enter synthetic
     *  aliases such as AnyRef into a package class without forcing it.
     *  Right now, the only usage is for the AnyRef alias in Definitions.
     */
    final private[core] def currentPackageDecls(implicit ctx: Context): MutableScope = myInfo match {
      case pinfo: SymbolLoaders.PackageLoader => pinfo.currentDecls
      case _ => unforcedDecls.openForMutations
    }

    /** If this is a synthetic opaque type alias, mark it as Deferred with bounds
     *  as given by the right hand side's `WithBounds` annotation, if one is present,
     *  or with empty bounds of the right kind, otherwise.
     *  At the same time, integrate the original alias as a refinement of the
     *  self type of the enclosing class.
     */
    final def normalizeOpaque()(implicit ctx: Context) = {
      def abstractRHS(tp: Type): Type = tp match {
        case tp: HKTypeLambda => tp.derivedLambdaType(resType = abstractRHS(tp.resType))
        case _ => defn.AnyType
      }
      if (isOpaqueAlias) {
        info match {
          case TypeAlias(alias) =>
            val (refiningAlias, bounds) = alias match {
              case AnnotatedType(alias1, Annotation.WithBounds(bounds)) =>
              	(alias1, bounds)
              case _ =>
              	(alias, TypeBounds(defn.NothingType, abstractRHS(alias)))
            }
            def refineSelfType(selfType: Type) =
              RefinedType(selfType, name, TypeAlias(refiningAlias))
            val enclClassInfo = owner.asClass.classInfo
            enclClassInfo.selfInfo match {
              case self: Type =>
                owner.info = enclClassInfo.derivedClassInfo(selfInfo = refineSelfType(self))
              case self: Symbol =>
                self.info = refineSelfType(self.info)
            }
            info = bounds
            setFlag(Deferred)
            typeRef.recomputeDenot()
          case _ =>
        }
      }
    }

    // ------ Names ----------------------------------------------

    /** The expanded name of this denotation. */
    final def expandedName(implicit ctx: Context): Name =
      if (name.is(ExpandedName) || isConstructor) name
      else name.expandedName(initial.owner)
        // need to use initial owner to disambiguate, as multiple private symbols with the same name
        // might have been moved from different origins into the same class

    /** The effective name with which the denoting symbol was created */
    final def originalName(implicit ctx: Context): Name = initial.effectiveName

    /** The owner with which the denoting symbol was created. */
    final def originalOwner(implicit ctx: Context): Symbol = initial.maybeOwner

    /** The encoded full path name of this denotation, where outer names and inner names
     *  are separated by `separator` strings as indicated by the given name kind.
     *  Drops package objects. Represents each term in the owner chain by a simple `_$`.
     */
    def fullNameSeparated(kind: QualifiedNameKind)(implicit ctx: Context): Name =
      maybeOwner.fullNameSeparated(kind, kind, name)

    /** The encoded full path name of this denotation (separated by `prefixKind`),
     *  followed by the separator implied by `kind` and the given `name`.
     *  Drops package objects. Represents each term in the owner chain by a simple `_$`.
     */
     def fullNameSeparated(prefixKind: QualifiedNameKind, kind: QualifiedNameKind, name: Name)(implicit ctx: Context): Name =
      if (symbol == NoSymbol || isEffectiveRoot || kind == FlatName && is(PackageClass))
        name
      else {
        var filler = ""
        var encl = symbol
        while (!encl.isClass && !encl.isPackageObject) {
          encl = encl.owner
          filler += "_$"
        }
        var prefix = encl.fullNameSeparated(prefixKind)
        if (kind.separator == "$")
          // duplicate scalac's behavior: don't write a double '$$' for module class members.
          prefix = prefix.exclude(ModuleClassName)
        def qualify(n: SimpleName) =
          kind(prefix.toTermName, if (filler.isEmpty) n else termName(filler + n))
        val fn = name replace {
          case name: SimpleName => qualify(name)
          case name @ AnyQualifiedName(_, _) => qualify(name.mangled.toSimpleName)
        }
        if (name.isTypeName) fn.toTypeName else fn.toTermName
      }

    /** The encoded flat name of this denotation, where joined names are separated by `separator` characters. */
    def flatName(implicit ctx: Context): Name = fullNameSeparated(FlatName)

    /** `fullName` where `.' is the separator character */
    def fullName(implicit ctx: Context): Name = fullNameSeparated(QualifiedName)

    /** The name given in an `@alpha` annotation if one is present, `name` otherwise */
    final def erasedName(implicit ctx: Context): Name =
      getAnnotation(defn.AlphaAnnot) match {
        case Some(ann) =>
          ann.arguments match {
            case Literal(Constant(str: String)) :: Nil =>
              if (isType) str.toTypeName else str.toTermName
            case _ => name
          }
        case _ => name
      }

    // ----- Tests -------------------------------------------------

    /** Is this denotation a type? */
    override def isType: Boolean = name.isTypeName

    /** Is this denotation a class? */
    final def isClass: Boolean = isInstanceOf[ClassDenotation]

    /** Is this denotation a non-trait class? */
    final def isRealClass(implicit ctx: Context): Boolean = isClass && !is(Trait)

    /** Cast to class denotation */
    final def asClass: ClassDenotation = asInstanceOf[ClassDenotation]

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    /** Make denotation not exist */
    final def markAbsent(): Unit =
      myInfo = NoType

    /** Is symbol known to not exist, or potentially not completed yet? */
    final def unforcedIsAbsent(implicit ctx: Context): Boolean =
      myInfo == NoType ||
      (this is (ModuleVal, butNot = Package)) && moduleClass.unforcedIsAbsent

    /** Is symbol known to not exist? */
    final def isAbsent(implicit ctx: Context): Boolean = {
      ensureCompleted()
      (myInfo `eq` NoType) ||
      (this is (ModuleVal, butNot = Package)) && moduleClass.isAbsent
    }

    /** Is this symbol the root class or its companion object? */
    final def isRoot: Boolean =
      (maybeOwner eq NoSymbol) && (name.toTermName == nme.ROOT || name == nme.ROOTPKG)

    /** Is this symbol the empty package class or its companion object? */
    final def isEmptyPackage(implicit ctx: Context): Boolean =
      name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
    final def isEffectiveRoot(implicit ctx: Context): Boolean = isRoot || isEmptyPackage

    /** Is this symbol an anonymous class? */
    final def isAnonymousClass(implicit ctx: Context): Boolean =
      isClass && initial.name.isAnonymousClassName

    final def isAnonymousFunction(implicit ctx: Context): Boolean =
      this.symbol.is(Method) && initial.name.isAnonymousFunctionName

    final def isAnonymousModuleVal(implicit ctx: Context): Boolean =
      this.symbol.is(ModuleVal) && initial.name.isAnonymousClassName

    /** Is this a synthetic method that represents conversions between representations of a value class
      *  These methods are generated in ExtensionMethods
      *  and used in ElimErasedValueType.
      */
    final def isValueClassConvertMethod(implicit ctx: Context): Boolean =
      name.toTermName == nme.U2EVT ||
      name.toTermName == nme.EVT2U

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(implicit ctx: Context): Boolean =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaValueClasses().contains(symbol)

    /** Is symbol a primitive numeric value class? */
    def isNumericValueClass(implicit ctx: Context): Boolean =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaNumericValueClasses().contains(symbol)

    /** Is symbol a class for which no runtime representation exists? */
    def isNotRuntimeClass(implicit ctx: Context): Boolean = defn.NotRuntimeClasses contains symbol

    /** Is this symbol a class representing a refinement? These classes
     *  are used only temporarily in Typer and Unpickler as an intermediate
     *  step for creating Refinement types.
     */
    final def isRefinementClass(implicit ctx: Context): Boolean =
      name == tpnme.REFINE_CLASS

    /** Is this symbol a package object or its module class? */
    def isPackageObject(implicit ctx: Context): Boolean =
      name.isPackageObjectName && (owner is Package) && (this is Module)

    /** Is this symbol an abstract type? */
    final def isAbstractType(implicit ctx: Context): Boolean = this is DeferredType

    /** Is this symbol an alias type? */
    final def isAliasType(implicit ctx: Context): Boolean = isAbstractOrAliasType && !(this is Deferred)

    /** Is this symbol an abstract or alias type? */
    final def isAbstractOrAliasType: Boolean = isType & !isClass

    /** Is this symbol an abstract type or type parameter? */
    final def isAbstractOrParamType(implicit ctx: Context): Boolean = this is DeferredOrTypeParam

    /** Is this symbol a user-defined opaque alias type? */
    def isOpaqueAlias(implicit ctx: Context): Boolean = is(Opaque) && !isClass

    /** Is this symbol a module that contains opaque aliases? */
    def containsOpaques(implicit ctx: Context): Boolean = is(Opaque) && isClass

    def seesOpaques(implicit ctx: Context): Boolean =
      containsOpaques ||
      is(Module, butNot = Package) && owner.containsOpaques

    /** Is this the denotation of a self symbol of some class?
     *  This is the case if one of two conditions holds:
     *  1. It is the symbol referred to in the selfInfo part of the ClassInfo
     *     which is the type of this symbol's owner.
     *  2. This symbol is owned by a class, it's selfInfo field refers to a type
     *     (indicating the self definition does not introduce a name), and the
     *     symbol's name is "_".
     *  TODO: Find a more robust way to characterize self symbols, maybe by
     *       spending a Flag on them?
     */
    final def isSelfSym(implicit ctx: Context): Boolean = owner.infoOrCompleter match {
      case ClassInfo(_, _, _, _, selfInfo) =>
        selfInfo == symbol ||
          selfInfo.isInstanceOf[Type] && name == nme.WILDCARD
      case _ => false
    }

    /** Is this definition contained in `boundary`?
     *  Same as `ownersIterator contains boundary` but more efficient.
     */
    final def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean = {
      def recur(sym: Symbol): Boolean =
        if (sym eq boundary) true
        else if (sym eq NoSymbol) false
        else if ((sym is PackageClass) && !(boundary is PackageClass)) false
        else recur(sym.owner)
      recur(symbol)
    }

    final def isProperlyContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean =
      symbol != boundary && isContainedIn(boundary)

    /** Is this denotation static (i.e. with no outer instance)? */
    final def isStatic(implicit ctx: Context): Boolean =
      (if (maybeOwner eq NoSymbol) isRoot else maybeOwner.originDenotation.isStaticOwner) ||
        myFlags.is(JavaStatic)

    /** Is this a package class or module class that defines static symbols? */
    final def isStaticOwner(implicit ctx: Context): Boolean =
      myFlags.is(ModuleClass) && (myFlags.is(PackageClass) || isStatic)

    /** Is this denotation defined in the same scope and compilation unit as that symbol? */
    final def isCoDefinedWith(other: Symbol)(implicit ctx: Context): Boolean =
      (this.effectiveOwner == other.effectiveOwner) &&
      (  !(this.effectiveOwner is PackageClass)
        || this.unforcedIsAbsent || other.unforcedIsAbsent
        || { // check if they are defined in the same file(or a jar)
           val thisFile = this.symbol.associatedFile
           val thatFile = other.associatedFile
           (  thisFile == null
           || thatFile == null
           || thisFile.path == thatFile.path // Cheap possibly wrong check, then expensive normalization
           || thisFile.canonicalPath == thatFile.canonicalPath
           )
         }
      )

    /** Is this a denotation of a stable term (or an arbitrary type)?
      * Terms are stable if they are idempotent (as in TreeInfo.Idempotent): that is, they always return the same value,
      * if any.
      *
      * A *member* is stable, basically, if it behaves like a field projection: that is, it projects a constant result
      * out of its owner.
      *
      * However, a stable member might not yet be initialized (if it is an object or anyhow lazy).
      * So the first call to a stable member might fail and/or produce side effects.
      */
    final def isStableMember(implicit ctx: Context): Boolean = {
      def isUnstableValue = is(UnstableValue) || info.isInstanceOf[ExprType]
      isType || is(StableRealizable) || !isUnstableValue
    }

    /** Is this a denotation of a class that does not have - either direct or inherited -
     *  initaliazion code?
     */
    def isNoInitsClass(implicit ctx: Context): Boolean =
      isClass &&
      (asClass.baseClasses.forall(_.is(NoInits)) || defn.isAssuredNoInits(symbol))

    /** Is this a "real" method? A real method is a method which is:
     *  - not an accessor
     *  - not an anonymous function
     */
    final def isRealMethod(implicit ctx: Context): Boolean =
      this.is(Method, butNot = Accessor) && !isAnonymousFunction

    /** Is this a getter? */
    final def isGetter(implicit ctx: Context): Boolean =
      (this is Accessor) && !originalName.isSetterName && !originalName.isScala2LocalSuffix

    /** Is this a setter? */
    final def isSetter(implicit ctx: Context): Boolean =
      (this is Accessor) &&
      originalName.isSetterName &&
      (!isCompleted || info.firstParamTypes.nonEmpty) // to avoid being fooled by   var x_= : Unit = ...

    /** is this a symbol representing an import? */
    final def isImport: Boolean = name == nme.IMPORT

    /** is this the constructor of a class? */
    final def isClassConstructor: Boolean = name == nme.CONSTRUCTOR

    /** Is this the constructor of a trait or a class */
    final def isConstructor: Boolean = name.isConstructorName

    /** Is this a local template dummmy? */
    final def isLocalDummy: Boolean = name.isLocalDummyName

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor(implicit ctx: Context): Boolean =
      isConstructor && owner.primaryConstructor == symbol

    /** Does this symbol denote the static constructor of its enclosing class? */
    final def isStaticConstructor(implicit ctx: Context): Boolean =
      name.isStaticConstructorName

    /** Is this a subclass of the given class `base`? */
    def isSubClass(base: Symbol)(implicit ctx: Context): Boolean = false

    /** Is this a subclass of `base`,
     *  and is the denoting symbol also different from `Null` or `Nothing`?
     *  @note  erroneous classes are assumed to derive from all other classes
     *         and all classes derive from them.
     */
    def derivesFrom(base: Symbol)(implicit ctx: Context): Boolean = false

    /** Is this symbol a class that extends `java.io.Serializable` ? */
    def isSerializable(implicit ctx: Context): Boolean =
      isClass && derivesFrom(defn.JavaSerializableClass)

    /** Is this symbol a class that extends `AnyVal`? */
    final def isValueClass(implicit ctx: Context): Boolean = {
      val di = initial
      di.isClass &&
      di.derivesFrom(defn.AnyValClass)(ctx.withPhase(di.validFor.firstPhaseId))
        // We call derivesFrom at the initial phase both because AnyVal does not exist
        // after Erasure and to avoid cyclic references caused by forcing denotations
    }

    /** Is this symbol a class references to which that are supertypes of null? */
    final def isNullableClass(implicit ctx: Context): Boolean =
      isClass && !isValueClass && !is(ModuleClass) && symbol != defn.NothingClass

    /** Is this definition accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     *  Everything is accessible if `pre` is `NoPrefix`.
     *  A symbol with type `NoType` is not accessible for any other prefix.
     */
    final def isAccessibleFrom(pre: Type, superAccess: Boolean = false, whyNot: StringBuffer = null)(implicit ctx: Context): Boolean = {

      /** Are we inside definition of `boundary`? */
      def accessWithin(boundary: Symbol) = ctx.owner.isContainedIn(boundary)

      /** Are we within definition of linked class of `boundary`? */
      def accessWithinLinked(boundary: Symbol) = {
        val linked = boundary.linkedClass
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /** Is `pre` the same as C.thisThis, where C is exactly the owner of this symbol,
       *  or, if this symbol is protected, a subclass of the owner?
       */
      def isCorrectThisType(pre: Type): Boolean = pre match {
        case pre: ThisType =>
          (pre.cls eq owner) || (this is Protected) && pre.cls.derivesFrom(owner)
        case pre: TermRef =>
          pre.symbol.moduleClass == owner
        case _ =>
          false
      }

      /** Is protected access to target symbol permitted? */
      def isProtectedAccessOK = {
        def fail(str: => String): Boolean = {
          if (whyNot != null) whyNot append str
          false
        }
        val cls = owner.enclosingSubClass
        if (!cls.exists)
          fail(
            i"""
               | Access to protected $this not permitted because enclosing ${ctx.owner.enclosingClass.showLocated}
               | is not a subclass of ${owner.showLocated} where target is defined""")
        else if (
          !(  isType // allow accesses to types from arbitrary subclasses fixes #4737
           || pre.derivesFrom(cls)
           || isConstructor
           || (owner is ModuleClass) // don't perform this check for static members
           ))
          fail(
            i"""
               | Access to protected ${symbol.show} not permitted because prefix type ${pre.widen.show}
               | does not conform to ${cls.showLocated} where the access takes place""")
        else true
      }

      if (pre eq NoPrefix) true
      else if (info eq NoType) false
      else {
        val boundary = accessBoundary(owner)

        (  boundary.isTerm
        || boundary.isRoot
        || (accessWithin(boundary) || accessWithinLinked(boundary)) &&
             (  !(this is Local)
             || isCorrectThisType(pre)
             )
        || (this is Protected) &&
             (  superAccess
             || pre.isInstanceOf[ThisType]
             || ctx.phase.erasedTypes
             || isProtectedAccessOK
             )
        )
      }
    }

    /** Do members of this symbol need translation via asSeenFrom when
     *  accessed via prefix `pre`?
     */
    def membersNeedAsSeenFrom(pre: Type)(implicit ctx: Context): Boolean =
      !(  this.isTerm
       || this.isStaticOwner && !this.seesOpaques
       || ctx.erasedTypes
       || (pre eq NoPrefix)
       || (pre eq thisType)
       )

    /** Is this symbol concrete, or that symbol deferred? */
    def isAsConcrete(that: Symbol)(implicit ctx: Context): Boolean =
      !(this is Deferred) || (that is Deferred)

    /** Does this symbol have defined or inherited default parameters? */
    def hasDefaultParams(implicit ctx: Context): Boolean =
      if (this is HasDefaultParams) true
      else if (this is NoDefaultParams) false
      else {
        val result = allOverriddenSymbols exists (_.hasDefaultParams)
        setFlag(if (result) InheritedDefaultParams else NoDefaultParams)
        result
      }

    /** Symbol is an owner that would be skipped by effectiveOwner. Skipped are
     *   - package objects
     *   - non-lazy valdefs
     */
    def isWeakOwner(implicit ctx: Context): Boolean =
      isPackageObject ||
      isTerm && !is(MethodOrLazy) && !isLocalDummy

    def isSkolem: Boolean = name == nme.SKOLEM

    def isInlineMethod(implicit ctx: Context): Boolean =
      is(InlineMethod, butNot = Accessor) &&
      !name.isUnapplyName  // unapply methods do not count as inline methods
                           // we need an inline flag on them only do that
                           // reduceProjection gets access to their rhs

    /** Is this a Scala 2 macro */
    final def isScala2Macro(implicit ctx: Context): Boolean = is(Macro) && symbol.owner.is(Scala2x)

    /** An erased value or an inline method, excluding @forceInline annotated methods.
     *  The latter have to be kept around to get to parity with Scala.
     *  This is necessary at least until we have full bootstrap. Right now
     *  dotty-bootstrapped involves running the Dotty compiler compiled with Scala 2 with
     *  a Dotty runtime library compiled with Dotty. If we erase @forceInline annotated
     *  methods, this means that the support methods in dotty.runtime.LazyVals vanish.
     *  But they are needed for running the lazy val implementations in the Scala-2 compiled compiler.
     */
    def isEffectivelyErased(implicit ctx: Context): Boolean =
      is(Erased) ||
      isInlineMethod && unforcedAnnotation(defn.ForceInlineAnnot).isEmpty

    /** ()T and => T types should be treated as equivalent for this symbol.
     *  Note: For the moment, we treat Scala-2 compiled symbols as loose matching,
     *  because the Scala library does not always follow the right conventions.
     *  Examples are: isWhole(), toInt(), toDouble() in BigDecimal, Numeric, RichInt, ScalaNumberProxy.
     */
    def matchNullaryLoosely(implicit ctx: Context): Boolean = {
      def test(sym: Symbol) =
        sym.is(JavaDefined) ||
        sym.owner == defn.AnyClass ||
        sym == defn.Object_clone ||
        sym.owner.is(Scala2x)
      this.exists && (test(symbol) || allOverriddenSymbols.exists(test))
    }

    // ------ access to related symbols ---------------------------------

    /* Modules and module classes are represented as follows:
     *
     * object X extends Y { def f() }
     *
     * <module> lazy val X: X$ = new X$
     * <module> class X$ extends Y { this: X.type => def f() }
     *
     * During completion, references to moduleClass and sourceModules are stored in
     * the completers.
     */
    /** If this a module, return the corresponding class, if this is a module, return itself,
     *  otherwise NoSymbol
     */
    final def moduleClass(implicit ctx: Context): Symbol = {
      def notFound = {
      	if (Config.showCompletions) println(s"missing module class for $name: $myInfo")
      	NoSymbol
      }
      if (this is ModuleVal)
        myInfo match {
          case info: TypeRef           => info.symbol
          case ExprType(info: TypeRef) => info.symbol // needed after uncurry, when module terms might be accessor defs
          case info: LazyType          => info.moduleClass
          case t: MethodType           =>
            t.resultType match {
              case info: TypeRef => info.symbol
              case _ => notFound
            }
          case _ => notFound
        }
      else if (this is ModuleClass)
        symbol
      else
        NoSymbol
    }

    /** If this a module class, return the corresponding module, if this is a module, return itself,
     *  otherwise NoSymbol
     */
    final def sourceModule(implicit ctx: Context): Symbol =
      if (this is ModuleClass)
        myInfo match {
          case ClassInfo(_, _, _, _, selfType) =>
            def sourceOfSelf(tp: TypeOrSymbol): Symbol = tp match {
              case tp: TermRef => tp.symbol
              case tp: Symbol => sourceOfSelf(tp.info)
              case tp: RefinedType => sourceOfSelf(tp.parent)
            }
            sourceOfSelf(selfType)
          case info: LazyType =>
            info.sourceModule
          case _ =>
            NoSymbol
        }
      else if (this is ModuleVal)
        symbol
      else
        NoSymbol

    /** The field accessed by this getter or setter, or if it does not exist, the getter */
    def accessedFieldOrGetter(implicit ctx: Context): Symbol = {
      val fieldName = if (isSetter) name.asTermName.getterName else name
      val d = owner.info.decl(fieldName)
      val field = d.suchThat(!_.is(Method)).symbol
      def getter = d.suchThat(_.info.isParameterless).symbol
      field orElse getter
    }

    /** The field accessed by a getter or setter, or
     *  if it does not exists, the getter of a setter, or
     *  if that does not exist the symbol itself.
     */
    def underlyingSymbol(implicit ctx: Context): Symbol =
      if (is(Accessor)) accessedFieldOrGetter orElse symbol else symbol

    /** The chain of owners of this denotation, starting with the denoting symbol itself */
    final def ownersIterator(implicit ctx: Context): Iterator[Symbol] = new Iterator[Symbol] {
      private[this] var current = symbol
      def hasNext = current.exists
      def next: Symbol = {
        val result = current
        current = current.owner
        result
      }
    }

    /** If this is a weak owner, its owner, otherwise the denoting symbol. */
    final def skipWeakOwner(implicit ctx: Context): Symbol =
      if (isWeakOwner) owner.skipWeakOwner else symbol

    /** The owner, skipping package objects and non-lazy valdefs. */
    final def effectiveOwner(implicit ctx: Context): Symbol = owner.skipWeakOwner

    /** The class containing this denotation.
     *  If this denotation is already a class, return itself
     *  Definitions flagged with JavaStatic are treated specially.
     *  Their enclosing class is not the lexically enclosing class,
     *  but in turn the enclosing class of the latter. This reflects
     *  the context created by `Context#superCallContext`, `Context#thisCallArgContext`
     *  for these definitions.
     *
     *  Note, that as packages have ClassSymbols, top level classes will have an `enclosingClass`
     *  with Package flag set.
     */
    final def enclosingClass(implicit ctx: Context): Symbol = {
      def enclClass(sym: Symbol, skip: Boolean): Symbol = {
        def newSkip = sym.is(JavaStaticTerm)
        if (!sym.exists)
          NoSymbol
        else if (sym.isClass)
          if (skip) enclClass(sym.owner, newSkip) else sym
        else
          enclClass(sym.owner, skip || newSkip)
      }
      enclClass(symbol, false)
    }

    /** A class that in source code would be lexically enclosing */
    final def lexicallyEnclosingClass(implicit ctx: Context): Symbol =
      if (!exists || isClass) symbol else owner.lexicallyEnclosingClass

    /** A symbol is effectively final if it cannot be overridden in a subclass */
    final def isEffectivelyFinal(implicit ctx: Context): Boolean =
      is(EffectivelyFinal) || !owner.isClass || owner.is(ModuleOrFinal) || owner.isAnonymousClass

    /** The class containing this denotation which has the given effective name. */
    final def enclosingClassNamed(name: Name)(implicit ctx: Context): Symbol = {
      val cls = enclosingClass
      if (cls.effectiveName == name || !cls.exists) cls else cls.owner.enclosingClassNamed(name)
    }

    /** The closest enclosing method containing this definition.
     *  A local dummy owner is mapped to the primary constructor of the class.
     */
    final def enclosingMethod(implicit ctx: Context): Symbol =
      if (this.is(Method)) symbol
      else if (this.isClass) primaryConstructor
      else if (this.exists) owner.enclosingMethod
      else NoSymbol

    /** The top-level class containing this denotation,
     *  except for a toplevel module, where its module class is returned.
     */
    final def topLevelClass(implicit ctx: Context): Symbol = {
      @tailrec def topLevel(d: SymDenotation): Symbol = {
        if (d.isTopLevelClass) d.symbol
        else topLevel(d.owner)
      }

      val sym = topLevel(this)
      if (sym.isClass) sym else sym.moduleClass
    }

    final def isTopLevelClass(implicit ctx: Context): Boolean =
      !this.exists || this.isEffectiveRoot || (this is PackageClass) || (this.owner is PackageClass)

    /** The package class containing this denotation */
    final def enclosingPackageClass(implicit ctx: Context): Symbol =
      if (this is PackageClass) symbol else owner.enclosingPackageClass

    /** Register target as a companion; overridden in ClassDenotation */
    def registerCompanion(target: Symbol)(implicit ctx: Context) = ()

    /** The registered companion; overridden in ClassDenotation */
    def registeredCompanion(implicit ctx: Context): Symbol = NoSymbol
    def registeredCompanion_=(c: Symbol): Unit = ()

    /** The module object with the same (term-) name as this class or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this module does not exist.
     */
    final def companionModule(implicit ctx: Context): Symbol =
      if (is(Module)) sourceModule
      else registeredCompanion.sourceModule

    private def companionType(implicit ctx: Context): Symbol =
      if (is(Package)) NoSymbol
      else if (is(ModuleVal)) moduleClass.denot.companionType
      else registeredCompanion

    /** The class with the same (type-) name as this module or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this class does not exist.
     */
    final def companionClass(implicit ctx: Context): Symbol =
      companionType.suchThat(_.isClass).symbol

    final def scalacLinkedClass(implicit ctx: Context): Symbol =
      if (this is ModuleClass) companionNamed(effectiveName.toTypeName)
      else if (this.isClass) companionNamed(effectiveName.moduleClassName).sourceModule.moduleClass
      else NoSymbol

    /** Find companion class symbol with given name, or NoSymbol if none exists.
     *  Three alternative strategies:
     *  1. If owner is a class, look in its members, otherwise
     *  2. If current compilation unit has a typed tree,
     *     determine the defining statement sequence and search its trees, otherwise
     *  3. If context has an enclosing scope which defines this symbol,
     *     lookup its companion in the same scope.
     */
    private def companionNamed(name: TypeName)(implicit ctx: Context): Symbol =
      if (owner.isClass)
        owner.unforcedDecls.lookup(name).suchThat(_.isCoDefinedWith(symbol)).symbol
      else if (!owner.exists || ctx.compilationUnit == null)
        NoSymbol
      else if (!ctx.compilationUnit.tpdTree.isEmpty)
        tpd.definingStats(symbol).iterator
          .map(tpd.definedSym)
          .find(_.name == name)
          .getOrElse(NoSymbol)
      else if (ctx.scope == null)
        NoSymbol
      else if (ctx.scope.lookup(this.name) == symbol)
        ctx.scope.lookup(name)
      else
        companionNamed(name)(ctx.outersIterator.dropWhile(_.scope eq ctx.scope).next())

    /** Is this symbol the same or a linked class of `sym`? */
    final def isLinkedWith(sym: Symbol)(implicit ctx: Context): Boolean =
      (symbol eq sym) || (linkedClass eq sym)

    /** If this is a class, the module class of its companion object.
     *  If this is a module class, its companion class.
     *  NoSymbol otherwise.
     */
    final def linkedClass(implicit ctx: Context): Symbol =
      if (this is ModuleClass) companionClass
      else if (this.isClass) companionModule.moduleClass
      else NoSymbol

    /** The class that encloses the owner of the current context
     *  and that is a subclass of this class. NoSymbol if no such class exists.
     */
    final def enclosingSubClass(implicit ctx: Context): Symbol =
      ctx.owner.ownersIterator.findSymbol(_.isSubClass(symbol))

    /** The alias of an opaque type alias that's stored in the self type of the
     *  containing object.
     */
    def opaqueAlias(implicit ctx: Context): Type = {
      def recur(tp: Type): Type = tp match {
        case RefinedType(parent, rname, TypeAlias(alias)) =>
          if (rname == name) alias else recur(parent)
        case _ =>
          NoType
      }
      recur(owner.asClass.classInfo.selfType)
    }

    /** The non-private symbol whose name and type matches the type of this symbol
     *  in the given class.
     *  @param inClass   The class containing the result symbol's definition
     *  @param site      The base type from which member types are computed
     *
     *  inClass <-- find denot.symbol      class C { <-- symbol is here
     *
     *                   site: Subtype of both inClass and C
     */
    final def matchingDecl(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = {
      var denot = inClass.info.nonPrivateDecl(name)
      if (denot.isTerm) // types of the same name always match
        denot = denot.matchingDenotation(site, site.memberInfo(symbol))
      denot.symbol
    }

    /** The non-private member of `site` whose name and type matches the type of this symbol
     */
    final def matchingMember(site: Type)(implicit ctx: Context): Symbol = {
      var denot = site.nonPrivateMember(name)
      if (denot.isTerm) // types of the same name always match
        denot = denot.matchingDenotation(site, site.memberInfo(symbol))
      denot.symbol
    }

    /** If false, this symbol cannot possibly participate in an override,
     *  either as overrider or overridee.
     */
    final def canMatchInheritedSymbols(implicit ctx: Context): Boolean =
      maybeOwner.isClass && memberCanMatchInheritedSymbols

    /** If false, this class member cannot possibly participate in an override,
     *  either as overrider or overridee.
     */
    final def memberCanMatchInheritedSymbols(implicit ctx: Context): Boolean =
      !isConstructor && !is(Private)

    /** The symbol, in class `inClass`, that is overridden by this denotation in class `siteClass`.*/
    final def overriddenSymbol(inClass: ClassSymbol, siteClass: ClassSymbol = owner.asClass)(implicit ctx: Context): Symbol =
      if (!canMatchInheritedSymbols && (owner ne inClass)) NoSymbol
      else matchingDecl(inClass, siteClass.thisType)

    /** All symbols overridden by this denotation. */
    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.info)

    /** Equivalent to `allOverriddenSymbols.headOption.getOrElse(NoSymbol)` but more efficient. */
    final def nextOverriddenSymbol(implicit ctx: Context): Symbol = {
      val overridden = allOverriddenSymbols
      if (overridden.hasNext)
        overridden.next
      else
        NoSymbol
    }

    /** Returns all matching symbols defined in parents of the selftype. */
    final def extendedOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.asClass.classInfo.selfType)

    private def overriddenFromType(tp: Type)(implicit ctx: Context): Iterator[Symbol] =
      tp.baseClasses match {
        case _ :: inherited => inherited.iterator.map(overriddenSymbol(_)).filter(_.exists)
        case Nil => Iterator.empty
      }

    /** The symbol overriding this symbol in given subclass `ofclazz`.
     *
     *  @param ofclazz is a subclass of this symbol's owner
     */
    final def overridingSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (canMatchInheritedSymbols) matchingDecl(inClass, inClass.thisType)
      else NoSymbol

    /** The symbol accessed by a super in the definition of this symbol when
     *  seen from class `base`. This symbol is always concrete.
     *  pre: `this.owner` is in the base class sequence of `base`.
     */
    final def superSymbolIn(base: Symbol)(implicit ctx: Context): Symbol = {
      @tailrec def loop(bcs: List[ClassSymbol]): Symbol = bcs match {
        case bc :: bcs1 =>
          val sym = matchingDecl(bcs.head, base.thisType)
            .suchThat(alt => !(alt is Deferred)).symbol
          if (sym.exists) sym else loop(bcs.tail)
        case _ =>
          NoSymbol
      }
      loop(base.info.baseClasses.dropWhile(owner != _).tail)
    }

    /** A member of class `base` is incomplete if
     *  (1) it is declared deferred or
     *  (2) it is abstract override and its super symbol in `base` is
     *      nonexistent or incomplete.
     */
    @tailrec final def isIncompleteIn(base: Symbol)(implicit ctx: Context): Boolean =
      (this is Deferred) ||
      (this is AbsOverride) && {
        val supersym = superSymbolIn(base)
        supersym == NoSymbol || supersym.isIncompleteIn(base)
      }

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base  The access boundary to assume if this symbol is protected
     */
    final def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = {
      val fs = flags
      if (fs is Private) owner
      else if (fs is StaticProtected) defn.RootClass
      else if (privateWithin.exists && !ctx.phase.erasedTypes) privateWithin
      else if (fs is Protected) base
      else defn.RootClass
    }

    /** The primary constructor of a class or trait, NoSymbol if not applicable. */
    def primaryConstructor(implicit ctx: Context): Symbol = NoSymbol

    /** The current declaration in this symbol's class owner that has the same name
     *  as this one, and, if there are several, also has the same signature.
     */
    def currentSymbol(implicit ctx: Context): Symbol = {
      val candidates = owner.info.decls.lookupAll(name)
      def test(sym: Symbol): Symbol =
        if (sym == symbol || sym.signature == signature) sym
        else if (candidates.hasNext) test(candidates.next)
        else NoSymbol
      if (candidates.hasNext) {
        val sym = candidates.next
        if (candidates.hasNext) test(sym) else sym
      }
      else NoSymbol
    }

    // ----- type-related ------------------------------------------------

    /** The type parameters of a class symbol, Nil for all other symbols */
    def typeParams(implicit ctx: Context): List[TypeSymbol] = Nil

    /** The type This(cls), where cls is this class, NoPrefix for all other symbols */
    def thisType(implicit ctx: Context): Type = NoPrefix

    def typeRef(implicit ctx: Context): TypeRef =
      TypeRef(owner.thisType, symbol)

    def termRef(implicit ctx: Context): TermRef =
      TermRef(owner.thisType, symbol)

    /** The typeRef applied to its own type parameters */
    def appliedRef(implicit ctx: Context): Type =
      typeRef.appliedTo(symbol.typeParams.map(_.typeRef))

    /** The NamedType representing this denotation at its original location.
     *  Same as either `typeRef` or `termRef` depending whether this denotes a type or not.
     */
    def namedType(implicit ctx: Context): NamedType =
      if (isType) typeRef else termRef

    /** The variance of this type parameter or type member as an Int, with
     *  +1 = Covariant, -1 = Contravariant, 0 = Nonvariant, or not a type parameter
     */
    final def variance(implicit ctx: Context): Int =
      if (this is Covariant) 1
      else if (this is Contravariant) -1
      else 0

    /** The flags to be used for a type parameter owned by this symbol.
     *  Overridden by ClassDenotation.
     */
    def typeParamCreationFlags: FlagSet = TypeParam

    override def toString: String = {
      val kindString =
        if (myFlags is ModuleClass) "module class"
        else if (isClass) "class"
        else if (isType) "type"
        else if (myFlags is Module) "module"
        else if (myFlags is Method) "method"
        else "val"
      s"$kindString $name"
    }

    // ----- Sanity checks and debugging */

    def debugString: String = toString + "#" + symbol.id // !!! DEBUG

    def hasSkolems(tp: Type): Boolean = tp match {
      case tp: SkolemType => true
      case tp: NamedType => hasSkolems(tp.prefix)
      case tp: RefinedType => hasSkolems(tp.parent) || hasSkolems(tp.refinedInfo)
      case tp: RecType => hasSkolems(tp.parent)
      case tp: TypeBounds => hasSkolems(tp.lo) || hasSkolems(tp.hi)
      case tp: TypeVar => hasSkolems(tp.inst)
      case tp: ExprType => hasSkolems(tp.resType)
      case tp: AppliedType => hasSkolems(tp.tycon) || tp.args.exists(hasSkolems)
      case tp: LambdaType => tp.paramInfos.exists(hasSkolems) || hasSkolems(tp.resType)
      case tp: AndType => hasSkolems(tp.tp1) || hasSkolems(tp.tp2)
      case tp: OrType  => hasSkolems(tp.tp1) || hasSkolems(tp.tp2)
      case tp: AnnotatedType => hasSkolems(tp.parent)
      case _ => false
    }

    def assertNoSkolems(tp: Type): Unit =
      if (!this.isSkolem)
        assert(!hasSkolems(tp), s"assigning type $tp containing skolems to $this")

    // ----- copies and transforms  ----------------------------------------

    protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)

    /** Copy this denotation, overriding selective fields */
    final def copySymDenotation(
      symbol: Symbol = this.symbol,
      owner: Symbol = this.owner,
      name: Name = this.name,
      initFlags: FlagSet = UndefinedFlags,
      info: Type = null,
      privateWithin: Symbol = null,
      annotations: List[Annotation] = null)(implicit ctx: Context): SymDenotation =
    { // simulate default parameters, while also passing implicit context ctx to the default values
      val initFlags1 = (if (initFlags != UndefinedFlags) initFlags else this.flags)
      val info1 = if (info != null) info else this.info
      if (ctx.isAfterTyper && changedClassParents(info, info1, completersMatter = false))
        assert(ctx.phase.changesParents, i"undeclared parent change at ${ctx.phase} for $this, was: $info, now: $info1")
      val privateWithin1 = if (privateWithin != null) privateWithin else this.privateWithin
      val annotations1 = if (annotations != null) annotations else this.annotations
      val d = ctx.SymDenotation(symbol, owner, name, initFlags1, info1, privateWithin1)
      d.annotations = annotations1
      d.registeredCompanion = registeredCompanion
      d
    }

    /** Copy mamberNames and baseData caches from given denotation, provided
     *  they are valid at given `phase`.
     */
    def copyCaches(from: SymDenotation, phase: Phase)(implicit ctx: Context): this.type = this

    /** Are `info1` and `info2` ClassInfo types with different parents?
     *  @param completersMatter  if `true`, consider parents changed if `info1` or `info2 `is a type completer
     */
    protected def changedClassParents(info1: Type, info2: Type, completersMatter: Boolean): Boolean =
      info2 match {
        case info2: ClassInfo =>
          info1 match {
            case info1: ClassInfo => info1.classParents ne info2.classParents
            case _ => completersMatter
          }
        case _ => completersMatter
      }

    override def initial: SymDenotation = super.initial.asSymDenotation

    /** Install this denotation as the result of the given denotation transformer. */
    override def installAfter(phase: DenotTransformer)(implicit ctx: Context): Unit =
      super.installAfter(phase)

    /** Apply a transformation `f` to all denotations in this group that start at or after
     *  given phase. Denotations are replaced while keeping the same validity periods.
     */
    override def transformAfter(phase: DenotTransformer, f: SymDenotation => SymDenotation)(implicit ctx: Context): Unit =
      super.transformAfter(phase, f)

    /** If denotation is private, remove the Private flag and expand the name if necessary */
    def ensureNotPrivate(implicit ctx: Context): SymDenotation =
      if (is(Private))
        copySymDenotation(name = expandedName, initFlags = this.flags &~ Private)
      else this
  }

  /** The contents of a class definition during a period
   */
  class ClassDenotation private[SymDenotations] (
    symbol: Symbol,
    maybeOwner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol)
    extends SymDenotation(symbol, maybeOwner, name, initFlags, initInfo, initPrivateWithin) {

    import util.LRUCache

    // ----- caches -------------------------------------------------------

    private[this] var myTypeParams: List[TypeSymbol] = null
    private[this] var fullNameCache: SimpleIdentityMap[QualifiedNameKind, Name] = SimpleIdentityMap.Empty

    private[this] var myMemberCache: LRUCache[Name, PreDenotation] = null
    private[this] var myMemberCachePeriod: Period = Nowhere

    /** A cache from types T to baseType(T, C) */
    type BaseTypeMap = java.util.IdentityHashMap[CachedType, Type]
    private[this] var myBaseTypeCache: BaseTypeMap = null
    private[this] var myBaseTypeCachePeriod: Period = Nowhere

    private var baseDataCache: BaseData = BaseData.None
    private var memberNamesCache: MemberNames = MemberNames.None

    private def memberCache(implicit ctx: Context): LRUCache[Name, PreDenotation] = {
      if (myMemberCachePeriod != ctx.period) {
        myMemberCache = new LRUCache
        myMemberCachePeriod = ctx.period
      }
      myMemberCache
    }

    private def baseTypeCache(implicit ctx: Context): BaseTypeMap = {
      if (!ctx.hasSameBaseTypesAs(myBaseTypeCachePeriod)) {
        myBaseTypeCache = new BaseTypeMap
        myBaseTypeCachePeriod = ctx.period
      }
      myBaseTypeCache
    }

    private def invalidateBaseDataCache() = {
      baseDataCache.invalidate()
      baseDataCache = BaseData.None
    }

    private def invalidateMemberNamesCache() = {
      memberNamesCache.invalidate()
      memberNamesCache = MemberNames.None
    }

    def invalidateBaseTypeCache(): Unit = {
      myBaseTypeCache = null
      myBaseTypeCachePeriod = Nowhere
    }

    override def copyCaches(from: SymDenotation, phase: Phase)(implicit ctx: Context): this.type = {
      from match {
        case from: ClassDenotation =>
          if (from.memberNamesCache.isValidAt(phase)) memberNamesCache = from.memberNamesCache
          if (from.baseDataCache.isValidAt(phase)) {
            baseDataCache = from.baseDataCache
            myBaseTypeCache = from.baseTypeCache
          }
        case _ =>
      }
      this
    }

    // ----- denotation fields and accessors ------------------------------

    if (initFlags is (Module, butNot = Package))
      assert(name.is(ModuleClassName), s"module naming inconsistency: ${name.debugString}")

    /** The symbol asserted to have type ClassSymbol */
    def classSymbol: ClassSymbol = symbol.asInstanceOf[ClassSymbol]

    /** The info asserted to have type ClassInfo */
    def classInfo(implicit ctx: Context): ClassInfo = info.asInstanceOf[ClassInfo]

    /** The type parameters in this class, in the order they appear in the current
     *  scope `decls`. This might be temporarily the incorrect order when
     *  reading Scala2 pickled info. The problem is fixed by `ensureTypeParamsInCorrectOrder`,
     *  which is called once an unpickled symbol has been completed.
     */
    private def typeParamsFromDecls(implicit ctx: Context) =
      unforcedDecls.filter(sym =>
        (sym is TypeParam) && sym.owner == symbol).asInstanceOf[List[TypeSymbol]]

    /** The type parameters of this class */
    override final def typeParams(implicit ctx: Context): List[TypeSymbol] = {
      if (myTypeParams == null)
        myTypeParams =
          if (ctx.erasedTypes || is(Module)) Nil // fast return for modules to avoid scanning package decls
          else {
            val di = initial
            if (this ne di) di.typeParams
            else infoOrCompleter match {
              case info: TypeParamsCompleter => info.completerTypeParams(symbol)
              case _ => typeParamsFromDecls
            }
          }
      myTypeParams
    }

    override protected[dotc] final def info_=(tp: Type): Unit = {
      if (changedClassParents(infoOrCompleter, tp, completersMatter = true))
        invalidateBaseDataCache()
      invalidateMemberNamesCache()
      myTypeParams = null // changing the info might change decls, and with it typeParams
      super.info_=(tp)
    }

    def classParents(implicit ctx: Context): List[Type] = info match {
      case classInfo: ClassInfo => classInfo.parents
      case _ => Nil
    }

    /** The symbol of the superclass, NoSymbol if no superclass exists */
    def superClass(implicit ctx: Context): Symbol = classParents match {
      case parent :: _ =>
        val cls = parent.classSymbol
        if (cls is Trait) NoSymbol else cls
      case _ =>
        NoSymbol
    }

    /** The explicitly given self type (self types of modules are assumed to be
     *  explcitly given here).
     */
    def givenSelfType(implicit ctx: Context): Type = classInfo.selfInfo match {
      case tp: Type => tp
      case self: Symbol => self.info
    }

   // ------ class-specific operations -----------------------------------

    private[this] var myThisType: Type = null

    /** The this-type depends on the kind of class:
     *  - for a package class `p`:  ThisType(TypeRef(Noprefix, p))
     *  - for a module class `m`: A term ref to m's source module.
     *  - for all other classes `c` with owner `o`: ThisType(TypeRef(o.thisType, c))
     */
    override def thisType(implicit ctx: Context): Type = {
      if (myThisType == null) myThisType = computeThisType
      myThisType
    }

    private def computeThisType(implicit ctx: Context): Type = {
      val cls = symbol.asType
      val pre = if (this is Package) NoPrefix else owner.thisType
      ThisType.raw(TypeRef(pre, cls))
    }

    private[this] var myTypeRef: TypeRef = null

    override def typeRef(implicit ctx: Context): TypeRef = {
      if (myTypeRef == null) myTypeRef = super.typeRef
      myTypeRef
    }

    override def appliedRef(implicit ctx: Context): Type = classInfo.appliedRef

    private def baseData(implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet) = {
      if (!baseDataCache.isValid) baseDataCache = BaseData.newCache()
      baseDataCache(this)
    }

    /** The base classes of this class in linearization order,
     *  with the class itself as first element.
     */
    def baseClasses(implicit onBehalf: BaseData, ctx: Context): List[ClassSymbol] =
      baseData._1

    /** A bitset that contains the superId's of all base classes */
    private def baseClassSet(implicit onBehalf: BaseData, ctx: Context): BaseClassSet =
      baseData._2

    def computeBaseData(implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet) = {
      def emptyParentsExpected =
        is(Package) || (symbol == defn.AnyClass) || ctx.erasedTypes && (symbol == defn.ObjectClass)
      if (classParents.isEmpty && !emptyParentsExpected)
        onBehalf.signalProvisional()
      val builder = new BaseDataBuilder
      def traverse(parents: List[Type]): Unit = parents match {
        case p :: parents1 =>
          p.classSymbol match {
            case pcls: ClassSymbol => builder.addAll(pcls.baseClasses)
            case _ => assert(isRefinementClass || p.isError || ctx.mode.is(Mode.Interactive), s"$this has non-class parent: $p")
          }
          traverse(parents1)
        case nil =>
      }
      traverse(classParents)
      (classSymbol :: builder.baseClasses, builder.baseClassSet)
    }

    final override def derivesFrom(base: Symbol)(implicit ctx: Context): Boolean =
      !isAbsent &&
      base.isClass &&
      (  (symbol eq base)
      || (baseClassSet contains base)
      )

    final override def isSubClass(base: Symbol)(implicit ctx: Context): Boolean =
      derivesFrom(base) ||
        base.isClass && (
          (symbol eq defn.NothingClass) ||
            (symbol eq defn.NullClass) && (base ne defn.NothingClass))

    /** Is it possible that a class inherits both `this` and `that`?
     *
     *  @note The test is based on single-class inheritance and the closed
     *        hierarchy of final classes.
     *
     *  @return The result may contain false positives, but never false negatives.
     */
    final def mayHaveCommonChild(that: ClassSymbol)(implicit ctx: Context): Boolean =
      !this.is(Final) && !that.is(Final) && (this.is(Trait) || that.is(Trait)) ||
        this.derivesFrom(that) || that.derivesFrom(this.symbol)

    final override def typeParamCreationFlags: FlagSet = ClassTypeParamCreationFlags

    /** Hook to do a pre-enter test. Overridden in PackageDenotation */
    protected def proceedWithEnter(sym: Symbol, mscope: MutableScope)(implicit ctx: Context): Boolean = true

    /** Enter a symbol in current scope, and future scopes of same denotation.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     *  @param scope   The scope in which symbol should be entered.
     *                 If this is EmptyScope, the scope is `decls`.
     */
    def enter(sym: Symbol, scope: Scope = EmptyScope)(implicit ctx: Context): Unit = {
      val mscope = scope match {
        case scope: MutableScope =>
          // if enter gets a scope as an argument,
          // than this is a scope that will eventually become decls of this symbol.
          // And this should only happen if this is first time the scope of symbol
          // is computed, ie symbol yet has no future.
          assert(this.nextInRun.validFor.code <= this.validFor.code)
          scope
        case _ => unforcedDecls.openForMutations
      }
      if (proceedWithEnter(sym, mscope)) {
        enterNoReplace(sym, mscope)
        val nxt = this.nextInRun
        if (nxt.validFor.code > this.validFor.code) {
          this.nextInRun.asSymDenotation.asClass.enter(sym)
        }
        if (defn.isScalaShadowingPackageClass(sym.owner))
          defn.ScalaPackageClass.enter(sym)  // ScalaShadowing members are mirrored in ScalaPackage
      }
    }

    /** Enter a symbol in given `scope` without potentially replacing the old copy. */
    def enterNoReplace(sym: Symbol, scope: MutableScope)(implicit ctx: Context): Unit = {
      scope.enter(sym)
      if (myMemberCache != null) myMemberCache.invalidate(sym.name)
      if (!sym.flagsUNSAFE.is(Private)) invalidateMemberNamesCache()
    }

    /** Replace symbol `prev` (if defined in current class) by symbol `replacement`.
     *  If `prev` is not defined in current class, do nothing.
     *  @pre `prev` and `replacement` have the same name.
     */
    def replace(prev: Symbol, replacement: Symbol)(implicit ctx: Context): Unit = {
      unforcedDecls.openForMutations.replace(prev, replacement)
      if (myMemberCache != null) myMemberCache.invalidate(replacement.name)
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(implicit ctx: Context): Unit = {
      info.decls.openForMutations.unlink(sym)
      if (myMemberCache != null) myMemberCache.invalidate(sym.name)
      if (!sym.flagsUNSAFE.is(Private)) invalidateMemberNamesCache()
    }

    /** Make sure the type parameters of this class appear in the order given
     *  by `typeParams` in the scope of the class. Reorder definitions in scope if necessary.
     */
    def ensureTypeParamsInCorrectOrder()(implicit ctx: Context): Unit = {
      val tparams = typeParams
      if (!ctx.erasedTypes && !typeParamsFromDecls.corresponds(tparams)(_.name == _.name)) {
        val decls = info.decls
        val decls1 = newScope
        for (tparam <- typeParams) decls1.enter(decls.lookup(tparam.name))
        for (sym <- decls) if (!tparams.contains(sym)) decls1.enter(sym)
        info = classInfo.derivedClassInfo(decls = decls1)
        myTypeParams = null
      }
    }

    /** All members of this class that have the given name.
     *  The elements of the returned pre-denotation all
     *  have existing symbols.
     */
    final def membersNamed(name: Name)(implicit ctx: Context): PreDenotation = {
      val privates = info.decls.denotsNamed(name, selectPrivate)
      privates union nonPrivateMembersNamed(name).filterDisjoint(privates)
    }

    /** All non-private members of this class that have the given name.
     *  The elements of the returned pre-denotation all
     *  have existing symbols.
     *  @param inherited  The method is called on a parent class from computeNPMembersNamed
     */
    final def nonPrivateMembersNamed(name: Name)(implicit ctx: Context): PreDenotation = {
      Stats.record("nonPrivateMembersNamed")
      if (Config.cacheMembersNamed) {
        var denots: PreDenotation = memberCache lookup name
        if (denots == null) {
          denots = computeNPMembersNamed(name)
          memberCache.enter(name, denots)
        } else if (Config.checkCacheMembersNamed) {
          val denots1 = computeNPMembersNamed(name)
          assert(denots.exists == denots1.exists, s"cache inconsistency: cached: $denots, computed $denots1, name = $name, owner = $this")
        }
        denots
      } else computeNPMembersNamed(name)
    }

    private[core] def computeNPMembersNamed(name: Name)(implicit ctx: Context): PreDenotation = /*>|>*/ Stats.track("computeNPMembersNamed") /*<|<*/ {
      Stats.record("computeNPMembersNamed after fingerprint")
      ensureCompleted()
      val ownDenots = info.decls.denotsNamed(name, selectNonPrivate)
      if (debugTrace) // DEBUG
        println(s"$this.member($name), ownDenots = $ownDenots")
      def collect(denots: PreDenotation, parents: List[Type]): PreDenotation = parents match {
        case p :: ps =>
          val denots1 = collect(denots, ps)
          p.classSymbol.denot match {
            case parentd: ClassDenotation =>
              denots1.union(
                parentd.nonPrivateMembersNamed(name)
                  .mapInherited(ownDenots, denots1, thisType))
            case _ =>
              denots1
          }
        case nil =>
          denots
      }
      if (name.isConstructorName) ownDenots
      else collect(ownDenots, classParents)
    }

    override final def findMember(name: Name, pre: Type, required: FlagConjunction, excluded: FlagSet)(implicit ctx: Context): Denotation = {
      val raw = if (excluded is Private) nonPrivateMembersNamed(name) else membersNamed(name)
      raw.filterWithFlags(required, excluded).asSeenFrom(pre).toDenot(pre)
    }

    /** Compute tp.baseType(this) */
    final def baseTypeOf(tp: Type)(implicit ctx: Context): Type = {
      val btrCache = baseTypeCache
      def inCache(tp: Type) = btrCache.get(tp) != null
      def record(tp: CachedType, baseTp: Type) = {
        if (Stats.monitored) {
          Stats.record("basetype cache entries")
          if (!baseTp.exists) Stats.record("basetype cache NoTypes")
        }
        if (!tp.isProvisional)
          btrCache.put(tp, baseTp)
        else
          btrCache.remove(tp) // Remove any potential sentinel value
      }

      def ensureAcyclic(baseTp: Type) = {
        if (baseTp `eq` NoPrefix) throw CyclicReference(this)
        baseTp
      }

      def recur(tp: Type): Type = try {
        tp match {
          case tp: CachedType =>
            val baseTp = btrCache.get(tp)
            if (baseTp != null) return ensureAcyclic(baseTp)
          case _ =>
        }
        if (Stats.monitored) {
          Stats.record("computeBaseType, total")
          Stats.record(s"computeBaseType, ${tp.getClass}")
        }
        val normed = tp.tryNormalize
        if (normed.exists) return recur(normed)

        tp match {
          case tp @ TypeRef(prefix, _) =>
            def foldGlb(bt: Type, ps: List[Type]): Type = ps match {
              case p :: ps1 => foldGlb(bt & recur(p), ps1)
              case _ => bt
            }

            def computeTypeRef = {
              btrCache.put(tp, NoPrefix)
              val tpSym = tp.symbol
              tpSym.denot match {
                case clsd: ClassDenotation =>
                  def isOwnThis = prefix match {
                    case prefix: ThisType => prefix.cls `eq` clsd.owner
                    case NoPrefix => true
                    case _ => false
                  }
                  val baseTp =
                    if (tpSym eq symbol)
                      tp
                    else if (isOwnThis)
                      if (clsd.baseClassSet.contains(symbol))
                        if (symbol.isStatic && symbol.typeParams.isEmpty) symbol.typeRef
                        else foldGlb(NoType, clsd.classParents)
                      else NoType
                    else
                      recur(clsd.typeRef).asSeenFrom(prefix, clsd.owner)
                  record(tp, baseTp)
                  baseTp
                case _ =>
                  val superTp = tp.superType
                  val baseTp = recur(superTp)
                  if (inCache(superTp))
                    record(tp, baseTp)
                  else
                    btrCache.remove(tp)
                  baseTp
              }
            }
            computeTypeRef

          case tp @ AppliedType(tycon, args) =>
            def computeApplied = {
              btrCache.put(tp, NoPrefix)
              val baseTp =
              	if (tycon.typeSymbol eq symbol) tp
              	else (tycon.typeParams: @unchecked) match {
                  case LambdaParam(_, _) :: _ =>
                    recur(tp.superType)
                  case tparams: List[Symbol @unchecked] =>
                    recur(tycon).substApprox(tparams, args)
                }
              record(tp, baseTp)
              baseTp
            }
            computeApplied

          case tp: TypeParamRef =>  // uncachable, since baseType depends on context bounds
            recur(ctx.typeComparer.bounds(tp).hi)

          case tp: TypeProxy =>
            def computeTypeProxy = {
              val superTp = tp.superType
              val baseTp = recur(superTp)
              tp match {
                case tp: CachedType if baseTp.exists && inCache(superTp) =>
                  record(tp, baseTp)
                case _ =>
              }
              baseTp
            }
            computeTypeProxy

          case tp: AndOrType =>
            def computeAndOrType = {
              val tp1 = tp.tp1
              val tp2 = tp.tp2
              val baseTp =
                if (symbol.isStatic && tp.derivesFrom(symbol) && symbol.typeParams.isEmpty)
                  symbol.typeRef
                else {
                  val baseTp1 = recur(tp1)
                  val baseTp2 = recur(tp2)
                  val combined = if (tp.isAnd) baseTp1 & baseTp2 else baseTp1 | baseTp2
                  combined match {
                    case combined: AndOrType
                    if (combined.tp1 eq tp1) && (combined.tp2 eq tp2) && (combined.isAnd == tp.isAnd) => tp
                    case _ => combined
                  }
                }
              if (baseTp.exists && inCache(tp1) && inCache(tp2)) record(tp, baseTp)
              baseTp
            }
            computeAndOrType

          case JavaArrayType(_) if symbol == defn.ObjectClass =>
            this.typeRef

          case _ =>
            NoType
        }
      }
      catch {
        case ex: Throwable =>
          btrCache.remove(tp)
          throw ex
      }


      /*>|>*/ trace.onDebug(s"$tp.baseType($this)") /*<|<*/ {
        Stats.record("baseTypeOf")
        recur(tp)
      }
    }

    def memberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] =
     if ((this is PackageClass) || !Config.cacheMemberNames)
        computeMemberNames(keepOnly) // don't cache package member names; they might change
      else {
        if (!memberNamesCache.isValid) memberNamesCache = MemberNames.newCache()
        memberNamesCache(keepOnly, this)
      }

    def computeMemberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] = {
      var names = Set[Name]()
      def maybeAdd(name: Name) = if (keepOnly(thisType, name)) names += name
      try {
        for (p <- classParents)
          for (name <- p.classSymbol.asClass.memberNames(keepOnly))
            maybeAdd(name)
        val ownSyms =
          if (keepOnly eq implicitFilter)
            if (this is Package) Iterator.empty
              // implicits in package objects are added by the overriding `memberNames` in `PackageClassDenotation`
            else info.decls.iterator filter (_ is ImplicitOrImpliedOrGiven)
          else info.decls.iterator
        for (sym <- ownSyms) maybeAdd(sym.name)
        names
      }
      catch {
        case ex: Throwable =>
          handleRecursive("member names", i"of $this", ex)
      }
    }

    override final def fullNameSeparated(kind: QualifiedNameKind)(implicit ctx: Context): Name = {
      val cached = fullNameCache(kind)
      if (cached != null) cached
      else {
        val fn = super.fullNameSeparated(kind)
        fullNameCache = fullNameCache.updated(kind, fn)
        fn
      }
    }

    // to avoid overloading ambiguities
    override def fullName(implicit ctx: Context): Name = super.fullName

    override def primaryConstructor(implicit ctx: Context): Symbol = {
      def constrNamed(cname: TermName) = info.decls.denotsNamed(cname).last.symbol
        // denotsNamed returns Symbols in reverse order of occurrence
      if (this.is(Package)) NoSymbol
      else constrNamed(nme.CONSTRUCTOR).orElse(constrNamed(nme.TRAIT_CONSTRUCTOR))
    }

    /** The term parameter accessors of this class.
     *  Both getters and setters are returned in this list.
     */
    def paramAccessors(implicit ctx: Context): List[Symbol] =
      unforcedDecls.filter(_.is(ParamAccessor))

    /** If this class has the same `decls` scope reference in `phase` and
     *  `phase.next`, install a new denotation with a cloned scope in `phase.next`.
     */
    def ensureFreshScopeAfter(phase: DenotTransformer)(implicit ctx: Context): Unit =
      if (ctx.phaseId != phase.next.id) ensureFreshScopeAfter(phase)(ctx.withPhase(phase.next))
      else {
        val prevCtx = ctx.withPhase(phase)
        val prevClassInfo = current(prevCtx).asInstanceOf[ClassDenotation].classInfo(prevCtx)
        val ClassInfo(pre, _, ps, decls, selfInfo) = classInfo
        if (prevClassInfo.decls eq decls)
          copySymDenotation(info = ClassInfo(pre, classSymbol, ps, decls.cloneScope, selfInfo))
            .copyCaches(this, phase.next)
            .installAfter(phase)
      }

    private[this] var myCompanion: Symbol = NoSymbol

    /** Register companion class */
    override def registerCompanion(companion: Symbol)(implicit ctx: Context) =
      if (companion.isClass && !unforcedIsAbsent && !companion.unforcedIsAbsent)
        myCompanion = companion

    override def registeredCompanion(implicit ctx: Context) = { ensureCompleted(); myCompanion }
    override def registeredCompanion_=(c: Symbol) = { myCompanion = c }
  }

  /** The denotation of a package class.
   *  It overrides ClassDenotation to take account of package objects when looking for members
   */
  final class PackageClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol)
    extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin) {

    private[this] var packageObjsCache: List[ClassDenotation] = _
    private[this] var packageObjsRunId: RunId = NoRunId

    /** The package objects in this class */
    def packageObjs(implicit ctx: Context): List[ClassDenotation] = {
      if (packageObjsRunId != ctx.runId) {
        packageObjsRunId = ctx.runId
        packageObjsCache = Nil // break cycle in case we are looking for package object itself
        packageObjsCache = {
          val pkgObjBuf = new mutable.ListBuffer[ClassDenotation]
          for (sym <- info.decls) { // don't use filter, since that loads classes with `$`s in their name
            val denot = sym.lastKnownDenotation  // don't use `sym.denot`, as this brings forward classes too early
            if (denot.isType && denot.name.isPackageObjectName)
              pkgObjBuf += sym.asClass.classDenot
          }
          pkgObjBuf.toList
        }
      }
      packageObjsCache
    }

    /** The package object (as a term symbol) in this package that might contain
     *  `sym` as a member.
     */
    def packageObjFor(sym: Symbol)(implicit ctx: Context): Symbol = {
      val owner = sym.maybeOwner
      if (owner.is(Package)) NoSymbol
      else if (owner.isPackageObject) owner.sourceModule
      else // owner could be class inherited by package object (until package object inheritance is removed)
        packageObjs.find(_.name == packageTypeName) match {
          case Some(pobj) => pobj.sourceModule
          case _ => NoSymbol
        }
    }

    /** Looks in both the package object and the package for members. The precise algorithm
     *  is as follows:
     *
     *  If this is the scala package look in the package first, and if nothing is found
     *  there, look in the package object second. Otherwise, look in the both the package object
     *  and the package and form a union of the results.
     *
     *  The reason for the special treatment of the scala package is that if we
     *  complete it too early, we freeze its superclass Any, so that no members can
     *  be entered in it. As a consequence, there should be no entry in the scala package
     *  object that hides a class or object in the scala package of the same name, because
     *  the behavior would then be unintuitive for such members.
     */
    override def computeNPMembersNamed(name: Name)(implicit ctx: Context): PreDenotation = {
      def recur(pobjs: List[ClassDenotation], acc: PreDenotation): PreDenotation = pobjs match {
        case pcls :: pobjs1 =>
          if (pcls.isCompleting) recur(pobjs1, acc)
          else {
            // A package object inherits members from `Any` and `Object` which
            // should not be accessible from the package prefix.
            val pmembers = pcls.computeNPMembersNamed(name).filterWithPredicate { d =>
              val owner = d.symbol.maybeOwner
              (owner ne defn.AnyClass) && (owner ne defn.ObjectClass)
            }
            recur(pobjs1, acc.union(pmembers))
          }
        case nil =>
          val directMembers = super.computeNPMembersNamed(name)
          if (acc.exists) acc.union(directMembers.filterWithPredicate(!_.symbol.isAbsent))
          else directMembers
      }
      if (symbol `eq` defn.ScalaPackageClass) {
        val denots = super.computeNPMembersNamed(name)
        if (denots.exists) denots
        else recur(packageObjs, NoDenotation)
      }
      else recur(packageObjs, NoDenotation)
    }

    /** The union of the member names of the package and the package object */
    override def memberNames(keepOnly: NameFilter)(implicit onBehalf: MemberNames, ctx: Context): Set[Name] = {
      def recur(pobjs: List[ClassDenotation], acc: Set[Name]): Set[Name] = pobjs match {
        case pcls :: pobjs1 =>
          recur(pobjs1, acc.union(pcls.memberNames(keepOnly)))
        case nil =>
          acc
      }
      recur(packageObjs, super.memberNames(keepOnly))
    }

    /** If another symbol with the same name is entered, unlink it,
     *  and, if symbol is a package object, invalidate the packageObj cache.
     *  @return  `sym` is not already entered
     */
    override def proceedWithEnter(sym: Symbol, mscope: MutableScope)(implicit ctx: Context): Boolean = {
      val entry = mscope.lookupEntry(sym.name)
      if (entry != null) {
        if (entry.sym == sym) return false
        mscope.unlink(entry)
        if (sym.name.isPackageObjectName) packageObjsRunId = NoRunId
      }
      true
    }

    /** Unlink all package members defined in `file` in a previous run. */
    def unlinkFromFile(file: AbstractFile)(implicit ctx: Context): Unit = {
      val scope = unforcedDecls.openForMutations
      for (sym <- scope.toList.iterator) {
        // We need to be careful to not force the denotation of `sym` here,
        // otherwise it will be brought forward to the current run.
        if (sym.defRunId != ctx.runId && sym.isClass && sym.asClass.assocFile == file)
          scope.unlink(sym, sym.lastKnownDenotation.name)
      }
    }
  }

  @sharable object NoDenotation
  extends SymDenotation(NoSymbol, NoSymbol, "<none>".toTermName, Permanent, NoType) {
    override def isType: Boolean = false
    override def isTerm: Boolean = false
    override def exists: Boolean = false
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def computeAsSeenFrom(pre: Type)(implicit ctx: Context): SingleDenotation = this
    override def mapInfo(f: Type => Type)(implicit ctx: Context): SingleDenotation = this
    NoSymbol.denot = this
    validFor = Period.allInRun(NoRunId)
  }

  // ---- Completion --------------------------------------------------------

  /** Instances of LazyType are carried by uncompleted symbols.
   *  Note: LazyTypes double up as (constant) functions from Symbol and
   *  from (TermSymbol, ClassSymbol) to LazyType. That way lazy types can be
   *  directly passed to symbol creation methods in Symbols that demand instances
   *  of these function types.
   */
  abstract class LazyType extends UncachedGroundType
    with (Symbol => LazyType)
    with ((TermSymbol, ClassSymbol) => LazyType) { self =>

    /** Sets all missing fields of given denotation */
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit

    def apply(sym: Symbol): LazyType = this
    def apply(module: TermSymbol, modcls: ClassSymbol): LazyType = this

    private[this] val NoSymbolFn = (_: Context) => NoSymbol
    private[this] var myDecls: Scope = EmptyScope
    private[this] var mySourceModuleFn: Context => Symbol = NoSymbolFn
    private[this] var myModuleClassFn: Context => Symbol = NoSymbolFn

    /** A proxy to this lazy type that keeps the complete operation
     *  but provides fresh slots for scope/sourceModule/moduleClass
     */
    def proxy: LazyType = new LazyType {
      override def complete(denot: SymDenotation)(implicit ctx: Context) = self.complete(denot)
    }

    /** The type parameters computed by the completer before completion has finished */
    def completerTypeParams(sym: Symbol)(implicit ctx: Context): List[TypeParamInfo] =
      if (sym is Touched) Nil // return `Nil` instead of throwing a cyclic reference
      else sym.info.typeParams

    def decls: Scope = myDecls
    def sourceModule(implicit ctx: Context): Symbol = mySourceModuleFn(ctx)
    def moduleClass(implicit ctx: Context): Symbol = myModuleClassFn(ctx)

    def withDecls(decls: Scope): this.type = { myDecls = decls; this }
    def withSourceModule(sourceModuleFn: Context => Symbol): this.type = { mySourceModuleFn = sourceModuleFn; this }
    def withModuleClass(moduleClassFn: Context => Symbol): this.type = { myModuleClassFn = moduleClassFn; this }
  }

  /** A subtrait of LazyTypes where completerTypeParams yields a List[TypeSymbol], which
   *  should be completed independently of the info.
   */
  trait TypeParamsCompleter extends LazyType {
    override def completerTypeParams(sym: Symbol)(implicit ctx: Context): List[TypeSymbol] =
      unsupported("completerTypeParams") // should be abstract, but Scala-2 will then compute the wrong type for it
  }

  /** A missing completer */
  @sharable class NoCompleter extends LazyType {
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = unsupported("complete")
  }

  object NoCompleter extends NoCompleter

  /** A lazy type for modules that points to the module class.
   *  Needed so that `moduleClass` works before completion.
   *  Completion of modules is always completion of the underlying
   *  module class, followed by copying the relevant fields to the module.
   */
  class ModuleCompleter(_moduleClass: ClassSymbol) extends LazyType {
    override def moduleClass(implicit ctx: Context): ClassSymbol = _moduleClass
    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      val from = moduleClass.denot.asClass
      denot.setFlag(from.flags.toTermFlags & RetainedModuleValFlags)
      denot.annotations = from.annotations filter (_.appliesToModule)
        // !!! ^^^ needs to be revised later. The problem is that annotations might
        // only apply to the module but not to the module class. The right solution
        // is to have the module class completer set the annotations of both the
        // class and the module.
      denot.info = moduleClass.typeRef
      denot.privateWithin = from.privateWithin
    }
  }

  /** A completer for missing references */
  class StubInfo() extends LazyType {

    def initializeToDefaults(denot: SymDenotation, errMsg: => Message)(implicit ctx: Context): Unit = {
      denot.info = denot match {
        case denot: ClassDenotation =>
          ClassInfo(denot.owner.thisType, denot.classSymbol, Nil, EmptyScope)
        case _ =>
          ErrorType(errMsg)
      }
      denot.privateWithin = NoSymbol
    }

    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      val sym = denot.symbol
      def errMsg = BadSymbolicReference(denot)
      ctx.error(errMsg, sym.sourcePos)
      if (ctx.debug) throw new scala.Error()
      initializeToDefaults(denot, errMsg)
    }
  }

  // ---- Caches for inherited info -----------------------------------------

  /** Base trait for caches that keep info dependent on inherited classes */
  trait InheritedCache {

    /** Is the cache valid in current period? */
    def isValid(implicit ctx: Context): Boolean

    /** is the cache valid in current run at given phase? */
    def isValidAt(phase: Phase)(implicit ctx: Context): Boolean

    /** Render invalid this cache and all cache that depend on it */
    def invalidate(): Unit
  }

  /** A cache for sets of member names, indexed by a NameFilter */
  trait MemberNames extends InheritedCache {
    def apply(keepOnly: NameFilter, clsd: ClassDenotation)
             (implicit onBehalf: MemberNames, ctx: Context): Set[Name]
  }

  object MemberNames {
    implicit val None: MemberNames = new InvalidCache with MemberNames {
      def apply(keepOnly: NameFilter, clsd: ClassDenotation)(implicit onBehalf: MemberNames, ctx: Context) = ???
    }
    def newCache()(implicit ctx: Context): MemberNames = new MemberNamesImpl(ctx.period)
  }

  /** A cache for baseclasses, as a sequence in linearization order and as a set that
   *  can be queried efficiently for containment.
   */
  trait BaseData extends InheritedCache {
    def apply(clsd: ClassDenotation)
             (implicit onBehalf: BaseData, ctx: Context): (List[ClassSymbol], BaseClassSet)
    def signalProvisional(): Unit
  }

  object BaseData {
    implicit val None: BaseData = new InvalidCache with BaseData {
      def apply(clsd: ClassDenotation)(implicit onBehalf: BaseData, ctx: Context) = ???
      def signalProvisional() = ()
    }
    def newCache()(implicit ctx: Context): BaseData = new BaseDataImpl(ctx.period)
  }

  private abstract class InheritedCacheImpl(val createdAt: Period) extends InheritedCache {
    protected def sameGroup(p1: Phase, p2: Phase): Boolean

    private[this] var dependent: WeakHashMap[InheritedCache, Unit] = null
    private[this] var checkedPeriod: Period = Nowhere

    protected def invalidateDependents() = {
      if (dependent != null) {
        val it = dependent.keySet.iterator()
        while (it.hasNext()) it.next().invalidate()
      }
      dependent = null
    }

    protected def addDependent(dep: InheritedCache) = {
      if (dependent == null) dependent = new WeakHashMap
      dependent.put(dep, ())
    }

    def isValidAt(phase: Phase)(implicit ctx: Context) =
      checkedPeriod == ctx.period ||
        createdAt.runId == ctx.runId &&
        createdAt.phaseId < ctx.phases.length &&
        sameGroup(ctx.phases(createdAt.phaseId), phase) &&
        { checkedPeriod = ctx.period; true }
  }

  private class InvalidCache extends InheritedCache {
    def isValid(implicit ctx: Context) = false
    def isValidAt(phase: Phase)(implicit ctx: Context) = false
    def invalidate(): Unit = ()
  }

  private class MemberNamesImpl(createdAt: Period) extends InheritedCacheImpl(createdAt) with MemberNames {
    private[this] var cache: SimpleIdentityMap[NameFilter, Set[Name]] = SimpleIdentityMap.Empty

    final def isValid(implicit ctx: Context): Boolean =
      cache != null && isValidAt(ctx.phase)

    private[this] var locked = false

    /** Computing parent member names might force parents, which could invalidate
     *  the cache itself. In that case we should cancel invalidation and
     *  proceed as usual. However, all cache entries should be cleared.
     */
    def invalidate(): Unit =
      if (cache != null)
        if (locked) cache = SimpleIdentityMap.Empty
        else {
          cache = null
          invalidateDependents()
        }

    def apply(keepOnly: NameFilter, clsd: ClassDenotation)(implicit onBehalf: MemberNames, ctx: Context) = {
      assert(isValid)
      val cached = cache(keepOnly)
      try
        if (cached != null) cached
        else {
          locked = true
          val computed =
            try clsd.computeMemberNames(keepOnly)(this, ctx)
            finally locked = false
          cache = cache.updated(keepOnly, computed)
          computed
        }
      finally addDependent(onBehalf)
    }

    def sameGroup(p1: Phase, p2: Phase) = p1.sameMembersStartId == p2.sameMembersStartId
  }

  private class BaseDataImpl(createdAt: Period) extends InheritedCacheImpl(createdAt) with BaseData {
    private[this] var cache: (List[ClassSymbol], BaseClassSet) = null

    private[this] var valid = true
    private[this] var locked = false
    private[this] var provisional = false

    final def isValid(implicit ctx: Context): Boolean = valid && isValidAt(ctx.phase)

    def invalidate(): Unit =
      if (valid && !locked) {
        cache = null
        valid = false
        invalidateDependents()
      }

    def signalProvisional() = provisional = true

    def apply(clsd: ClassDenotation)(implicit onBehalf: BaseData, ctx: Context)
        : (List[ClassSymbol], BaseClassSet) = {
      assert(isValid)
      try {
        if (cache != null) cache
        else {
          if (locked) throw CyclicReference(clsd)
          locked = true
          provisional = false
          val computed =
            try clsd.computeBaseData(this, ctx)
            finally locked = false
          if (!provisional) cache = computed
          else onBehalf.signalProvisional()
          computed
        }
      }
      finally addDependent(onBehalf)
    }

    def sameGroup(p1: Phase, p2: Phase) = p1.sameParentsStartId == p2.sameParentsStartId
  }

  class BaseClassSet(val classIds: Array[Int]) extends AnyVal {
    def contains(sym: Symbol, limit: Int): Boolean = {
      val id = sym.id
      var i = 0
      while (i < limit && classIds(i) != id) i += 1
      i < limit && {
        if (i > 0) {
          val t = classIds(i)
          classIds(i) = classIds(i - 1)
          classIds(i - 1) = t
        }
        true
      }
    }
    def contains(sym: Symbol): Boolean = contains(sym, classIds.length)
  }

  object BaseClassSet {
    def apply(bcs: List[ClassSymbol]): BaseClassSet =
      new BaseClassSet(bcs.toArray.map(_.id))
  }

  /** A class to combine base data from parent types */
  class BaseDataBuilder {
    private[this] var classes: List[ClassSymbol] = Nil
    private[this] var classIds = new Array[Int](32)
    private[this] var length = 0

    private def resize(size: Int) = {
      val classIds1 = new Array[Int](size)
      System.arraycopy(classIds, 0, classIds1, 0, classIds.length min size)
      classIds = classIds1
    }

    private def add(sym: Symbol): Unit = {
      if (length == classIds.length) resize(length * 2)
      classIds(length) = sym.id
      length += 1
    }

    def addAll(bcs: List[ClassSymbol]): this.type = {
      val len = length
      bcs match {
        case bc :: bcs1 =>
          addAll(bcs1)
          if (!new BaseClassSet(classIds).contains(bc, len)) {
            add(bc)
            classes = bc :: classes
          }
        case nil =>
      }
      this
    }

    def baseClassSet: BaseClassSet = {
      if (length != classIds.length) resize(length)
      new BaseClassSet(classIds)
    }

    def baseClasses: List[ClassSymbol] = classes
  }

  private val packageTypeName = ModuleClassName(nme.PACKAGE).toTypeName

  @sharable private[this] var indent = 0 // for completions printing
}
