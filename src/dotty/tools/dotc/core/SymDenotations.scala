package dotty.tools
package dotc
package core

import Periods._, Contexts._, Symbols._, Denotations._, Names._, NameOps._, Annotations._
import Types._, Flags._, Decorators._, DenotTransformers._, StdNames._, Scopes._
import NameOps._
import Scopes.Scope
import collection.mutable
import collection.immutable.BitSet
import scala.reflect.io.AbstractFile
import Decorators.SymbolIteratorDecorator
import ast._
import annotation.tailrec
import CheckRealizable._
import util.SimpleMap
import util.Stats
import config.Config
import config.Printers._

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
        if (initFlags is Package) new PackageClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin, ctx.runId)
        else new ClassDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin, ctx.runId)
      else new SymDenotation(symbol, owner, name, initFlags, initInfo, initPrivateWithin)
    result.validFor = stablePeriod
    result
  }

  def stillValid(denot: SymDenotation): Boolean =
    if (denot.is(ValidForever) || denot.isRefinementClass) true
    else {
      val initial = denot.initial
      val firstPhaseId = initial.validFor.firstPhaseId.max(ctx.typerPhase.id)
      if ((initial ne denot) || ctx.phaseId != firstPhaseId)
        ctx.withPhase(firstPhaseId).stillValidInOwner(initial.asSymDenotation)
      else
        stillValidInOwner(denot)
    }

  private[SymDenotations] def stillValidInOwner(denot: SymDenotation): Boolean = try {
    val owner = denot.owner.denot
    stillValid(owner) && (
      !owner.isClass
      || owner.isRefinementClass
      || (owner.unforcedDecls.lookupAll(denot.name) contains denot.symbol)
      || denot.isSelfSym)
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
        def explainSym(msg: String) = explain(s"$msg\n defined = ${denot.definedPeriodsString}")
        if (denot.is(ValidForever) || denot.isRefinementClass) true
        else {
          implicit val ctx: Context = this
          val initial = denot.initial
          if ((initial ne denot) || ctx.phaseId != initial.validFor.firstPhaseId) {
            ctx.withPhase(initial.validFor.firstPhaseId).traceInvalid(initial.asSymDenotation)
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
}

object SymDenotations {

  /** A sym-denotation represents the contents of a definition
   *  during a period.
   */
  class SymDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    final val name: Name,
    initFlags: FlagSet,
    final val initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol) {

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
    private[this] var myInfo: Type = initInfo
    private[this] var myPrivateWithin: Symbol = initPrivateWithin
    private[this] var myAnnotations: List[Annotation] = Nil

    /** The owner of the symbol; overridden in NoDenotation */
    def owner: Symbol = ownerIfExists

    /** Same as owner, except returns NoSymbol for NoSymbol */
    def maybeOwner: Symbol = if (exists) owner else NoSymbol

    /** The flag set */
    final def flags(implicit ctx: Context): FlagSet = { ensureCompleted(); myFlags }

    /** The flag set without forcing symbol completion.
     *  Should be used only for printing.
     */
    private[dotc] final def flagsUNSAFE = myFlags

    /** Adapt flag set to this denotation's term or type nature */
    private def adaptFlags(flags: FlagSet) = if (isType) flags.toTypeFlags else flags.toTermFlags

    /** Update the flag set */
    final def flags_=(flags: FlagSet): Unit =
      myFlags = adaptFlags(flags)

    /** Set given flags(s) of this denotation */
    final def setFlag(flags: FlagSet): Unit = { myFlags |= flags }

    /** Unset given flags(s) of this denotation */
    final def resetFlag(flags: FlagSet): Unit = { myFlags &~= flags }

    /** Set applicable flags from `flags` which is a subset of {NoInits, PureInterface} */
    final def setApplicableFlags(flags: FlagSet): Unit = {
      val mask = if (myFlags.is(Trait)) NoInitsInterface else NoInits
      setFlag(flags & mask)
    }

    /** Has this denotation one of the flags in `fs` set? */
    final def is(fs: FlagSet)(implicit ctx: Context) = {
      (if (fs <= FromStartFlags) myFlags else flags) is fs
    }

    /** Has this denotation one of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def is(fs: FlagSet, butNot: FlagSet)(implicit ctx: Context) =
      (if (fs <= FromStartFlags && butNot <= FromStartFlags) myFlags else flags) is (fs, butNot)

    /** Has this denotation all of the flags in `fs` set? */
    final def is(fs: FlagConjunction)(implicit ctx: Context) =
      (if (fs <= FromStartFlags) myFlags else flags) is fs

    /** Has this denotation all of the flags in `fs` set, whereas none of the flags
     *  in `butNot` are set?
     */
    final def is(fs: FlagConjunction, butNot: FlagSet)(implicit ctx: Context) =
      (if (fs <= FromStartFlags && butNot <= FromStartFlags) myFlags else flags) is (fs, butNot)

    /** The type info.
     *  The info is an instance of TypeType iff this is a type denotation
     *  Uncompleted denotations set myInfo to a LazyType.
     */
    final def info(implicit ctx: Context): Type = myInfo match {
      case myInfo: LazyType => completeFrom(myInfo); info
      case _ => myInfo
    }

    /** The type info, or, if symbol is not yet completed, the completer */
    final def infoOrCompleter = myInfo

    /** Optionally, the info if it is completed */
    final def unforcedInfo: Option[Type] = myInfo match {
      case myInfo: LazyType => None
      case _ => Some(myInfo)
    }

    private def completeFrom(completer: LazyType)(implicit ctx: Context): Unit = {
      if (completions ne noPrinter) {
        completions.println(i"${"  " * indent}completing ${if (isType) "type" else "val"} $name")
        indent += 1
      }
      if (myFlags is Touched) throw CyclicReference(this)
      myFlags |= Touched

      // completions.println(s"completing ${this.debugString}")
      try completer.complete(this)(ctx.withPhase(validFor.firstPhaseId))
      catch {
        case ex: CyclicReference =>
          completions.println(s"error while completing ${this.debugString}")
          throw ex
      }
      finally
        if (completions ne noPrinter) {
          indent -= 1
          completions.println(i"${"  " * indent}completed $name in $owner")
        }
      // completions.println(s"completed ${this.debugString}")
    }

    protected[dotc] def info_=(tp: Type) = {
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
      if (Config.checkNoSkolemsInInfo) assertNoSkolems(initInfo)
      myInfo = tp
    }

    /** The name, except
     *   - if this is a module class, strip the module class suffix
     *   - if this is a companion object with a clash-avoiding name, strip the
     *     "avoid clash" suffix
     */
    def effectiveName(implicit ctx: Context) =
      if (this is ModuleClass) name.stripModuleClassSuffix
      else name.stripAvoidClashSuffix

    /** The privateWithin boundary, NoSymbol if no boundary is given.
     */
    final def privateWithin(implicit ctx: Context): Symbol = { ensureCompleted(); myPrivateWithin }

    /** Set privateWithin. */
    protected[core] final def privateWithin_=(sym: Symbol): Unit =
      myPrivateWithin = sym

    /** The annotations of this denotation */
    final def annotations(implicit ctx: Context): List[Annotation] = {
      ensureCompleted(); myAnnotations
    }

    /** Update the annotations of this denotation */
    private[core] final def annotations_=(annots: List[Annotation]): Unit =
      myAnnotations = annots

    /** Does this denotation have an annotation matching the given class symbol? */
    final def hasAnnotation(cls: Symbol)(implicit ctx: Context) =
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

    /** Add given annotation to the annotations of this denotation */
    final def addAnnotation(annot: Annotation): Unit =
      annotations = annot :: myAnnotations

    /** Remove annotation with given class from this denotation */
    final def removeAnnotation(cls: Symbol)(implicit ctx: Context): Unit =
      annotations = myAnnotations.filterNot(_ matches cls)

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
      case pinfo: SymbolLoaders # PackageLoader => pinfo.currentDecls
      case _ => unforcedDecls.openForMutations
    }

    // ------ Names ----------------------------------------------

    /** The expanded name of this denotation. */
    final def expandedName(implicit ctx: Context) =
      if (is(ExpandedName) || isConstructor) name
      else {
        def legalize(name: Name): Name = // JVM method names may not contain `<' or `>' characters
          if (is(Method)) name.replace('<', '(').replace('>', ')') else name
        legalize(name.expandedName(initial.asSymDenotation.owner))
      }
        // need to use initial owner to disambiguate, as multiple private symbols with the same name
        // might have been moved from different origins into the same class

    /** The name with which the denoting symbol was created */
    final def originalName(implicit ctx: Context) = {
      val d = initial.asSymDenotation
      if (d is ExpandedName) d.name.unexpandedName else d.name // !!!DEBUG, was: effectiveName
    }

    /** The encoded full path name of this denotation, where outer names and inner names
     *  are separated by `separator` strings.
     *  Never translates expansions of operators back to operator symbol.
     *  Drops package objects. Represents terms in the owner chain by a simple `~`.
     *  (Note: scalac uses nothing to represent terms, which can cause name clashes
     *   between same-named definitions in different enclosing methods. Before this commit
     *   we used `$' but this can cause ambiguities with the class separator '$').
     *  A separator "" means "flat name"; the real separator in this case is "$" and
     *  enclosing packages do not form part of the name.
     */
    def fullNameSeparated(separator: String)(implicit ctx: Context): Name = {
      var sep = separator
      var stopAtPackage = false
      if (sep.isEmpty) {
        sep = "$"
        stopAtPackage = true
      }
      if (symbol == NoSymbol ||
          owner == NoSymbol ||
          owner.isEffectiveRoot ||
          stopAtPackage && owner.is(PackageClass)) name
      else {
        var encl = owner
        while (!encl.isClass && !encl.isPackageObject) {
          encl = encl.owner
          sep += "~"
        }
        if (owner.is(ModuleClass, butNot = Package) && sep == "$") sep = "" // duplicate scalac's behavior: don't write a double '$$' for module class members.
        val fn = encl.fullNameSeparated(separator) ++ sep ++ name
        if (isType) fn.toTypeName else fn.toTermName
      }
    }

    /** The encoded flat name of this denotation, where joined names are separated by `separator` characters. */
    def flatName(implicit ctx: Context): Name = fullNameSeparated("")

    /** `fullName` where `.' is the separator character */
    def fullName(implicit ctx: Context): Name = fullNameSeparated(".")

    // ----- Tests -------------------------------------------------

    /** Is this denotation a type? */
    override def isType: Boolean = name.isTypeName

    /** Is this denotation a class? */
    final def isClass: Boolean = isInstanceOf[ClassDenotation]

    /** Is this denotation a non-trait class? */
    final def isRealClass(implicit ctx: Context) = isClass && !is(Trait)

    /** Cast to class denotation */
    final def asClass: ClassDenotation = asInstanceOf[ClassDenotation]

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    /** Make denotation not exist */
    final def markAbsent(): Unit =
      myInfo = NoType

    /** Is symbol known to not exist? */
    final def isAbsent(implicit ctx: Context): Boolean =
      myInfo == NoType ||
      (this is (ModuleVal, butNot = Package)) && moduleClass.isAbsent

    /** Is this symbol the root class or its companion object? */
    final def isRoot: Boolean =
      (name.toTermName == nme.ROOT || name == nme.ROOTPKG) && (owner eq NoSymbol)

    /** Is this symbol the empty package class or its companion object? */
    final def isEmptyPackage(implicit ctx: Context): Boolean =
      name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
    final def isEffectiveRoot(implicit ctx: Context) = isRoot || isEmptyPackage

    /** Is this symbol an anonymous class? */
    final def isAnonymousClass(implicit ctx: Context): Boolean =
      isClass && (initial.asSymDenotation.name startsWith tpnme.ANON_CLASS)

    final def isAnonymousFunction(implicit ctx: Context) =
      this.symbol.is(Method) && (initial.asSymDenotation.name startsWith nme.ANON_FUN)

    final def isAnonymousModuleVal(implicit ctx: Context) =
      this.symbol.is(ModuleVal) && (initial.asSymDenotation.name startsWith nme.ANON_CLASS)

    /** Is this a companion class method or companion object method?
     *  These methods are generated by Symbols#synthesizeCompanionMethod
     *  and used in SymDenotations#companionClass and
     *  SymDenotations#companionModule .
     */
    final def isCompanionMethod(implicit ctx: Context) =
      name.toTermName == nme.COMPANION_CLASS_METHOD ||
      name.toTermName == nme.COMPANION_MODULE_METHOD

    /** Is this a syntetic method that represents conversions between representations of a value class
      *  These methods are generated in ExtensionMethods
      *  and used in ElimErasedValueType.
      */
    final def isValueClassConvertMethod(implicit ctx: Context) =
      name.toTermName == nme.U2EVT ||
      name.toTermName == nme.EVT2U

    /** Is symbol a primitive value class? */
    def isPrimitiveValueClass(implicit ctx: Context) =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaValueClasses().contains(symbol)

    /** Is symbol a primitive numeric value class? */
    def isNumericValueClass(implicit ctx: Context) =
      maybeOwner == defn.ScalaPackageClass && defn.ScalaNumericValueClasses().contains(symbol)

    /** Is symbol a phantom class for which no runtime representation exists? */
    def isPhantomClass(implicit ctx: Context) = defn.PhantomClasses contains symbol

    /** Is this symbol a class representing a refinement? These classes
     *  are used only temporarily in Typer and Unpickler as an intermediate
     *  step for creating Refinement types.
     */
    final def isRefinementClass(implicit ctx: Context): Boolean =
      name.decode == tpnme.REFINE_CLASS

    /** is this symbol a trait representing a type lambda? */
    final def isLambdaTrait(implicit ctx: Context): Boolean =
      isClass && name.startsWith(tpnme.hkLambdaPrefix) && owner == defn.ScalaPackageClass

    /** Is this symbol a package object or its module class? */
    def isPackageObject(implicit ctx: Context): Boolean = {
      val poName = if (isType) nme.PACKAGE_CLS else nme.PACKAGE
      (name.toTermName == poName) && (owner is Package) && (this is Module)
    }

    /** Is this symbol an abstract type? */
    final def isAbstractType(implicit ctx: Context) = isType && (this is Deferred)

    /** Is this symbol an alias type? */
    final def isAliasType(implicit ctx: Context) = isAbstractOrAliasType && !(this is Deferred)

    /** Is this symbol an abstract or alias type? */
    final def isAbstractOrAliasType = isType & !isClass

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
    final def isSelfSym(implicit ctx: Context) = owner.infoOrCompleter match {
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
    final def isStatic(implicit ctx: Context) =
      (this is JavaStatic) || this.exists && owner.isStaticOwner || this.isRoot

    /** Is this a package class or module class that defines static symbols? */
    final def isStaticOwner(implicit ctx: Context): Boolean =
      (this is PackageClass) || (this is ModuleClass) && isStatic

    /** Is this denotation defined in the same scope and compilation unit as that symbol? */
    final def isCoDefinedWith(that: Symbol)(implicit ctx: Context) =
      (this.effectiveOwner == that.effectiveOwner) &&
      (  !(this.effectiveOwner is PackageClass)
        || this.isAbsent || that.isAbsent
        || { // check if they are defined in the same file(or a jar)
           val thisFile = this.symbol.associatedFile
           val thatFile = that.symbol.associatedFile
           (  thisFile == null
           || thatFile == null
           || thisFile.path == thatFile.path // Cheap possibly wrong check, then expensive normalization
           || thisFile.canonicalPath == thatFile.canonicalPath
           )
         }
      )

    /** Is this a denotation of a stable term (or an arbitrary type)? */
    final def isStable(implicit ctx: Context) =
      isType || is(Stable) || !(is(UnstableValue) || info.isInstanceOf[ExprType])

    /** Is this a "real" method? A real method is a method which is:
     *  - not an accessor
     *  - not a label
     *  - not an anonymous function
     *  - not a companion method
     */
    final def isRealMethod(implicit ctx: Context) =
      this.is(Method, butNot = AccessorOrLabel) &&
        !isAnonymousFunction &&
        !isCompanionMethod

    /** Is this a getter? */
    final def isGetter(implicit ctx: Context) =
      (this is Accessor) && !originalName.isSetterName && !originalName.isScala2LocalSuffix

    /** Is this a setter? */
    final def isSetter(implicit ctx: Context) =
      (this is Accessor) &&
      originalName.isSetterName &&
      (!isCompleted || info.firstParamTypes.nonEmpty) // to avoid being fooled by   var x_= : Unit = ...

    /** is this the constructor of a class? */
    final def isClassConstructor = name == nme.CONSTRUCTOR

    /** Is this the constructor of a trait? */
    final def isImplClassConstructor = name == nme.TRAIT_CONSTRUCTOR

    /** Is this the constructor of a trait or a class */
    final def isConstructor = name.isConstructorName

    /** Is this a local template dummmy? */
    final def isLocalDummy: Boolean = name.isLocalDummyName

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor(implicit ctx: Context) =
      isConstructor && owner.primaryConstructor == symbol

    /** Does this symbol denote the static constructor of its enclosing class? */
    final def isStaticConstructor(implicit ctx: Context) =
      name.isStaticConstructorName

    /** Is this a subclass of the given class `base`? */
    def isSubClass(base: Symbol)(implicit ctx: Context) = false

    /** Is this a subclass of `base`,
     *  and is the denoting symbol also different from `Null` or `Nothing`?
     *  @note  erroneous classes are assumed to derive from all other classes
     *         and all classes derive from them.
     */
    def derivesFrom(base: Symbol)(implicit ctx: Context) = false

    /** Is this symbol a class that extends `AnyVal`? */
    final def isValueClass(implicit ctx: Context): Boolean = {
      val di = this.initial.asSymDenotation
      di.isClass &&
      di.derivesFrom(defn.AnyValClass)(ctx.withPhase(di.validFor.firstPhaseId))
        // We call derivesFrom at the initial phase both because AnyVal does not exist
        // after Erasure and to avoid cyclic references caused by forcing denotations
    }

    /** Is this symbol a class references to which that are supertypes of null? */
    final def isNullableClass(implicit ctx: Context): Boolean =
      isClass && !isValueClass && !(this is ModuleClass) && symbol != defn.NothingClass

    /** Is this definition accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     *  Everything is accessible if `pre` is `NoPrefix`.
     *  A symbol with type `NoType` is not accessible for any other prefix.
     */
    final def isAccessibleFrom(pre: Type, superAccess: Boolean = false, whyNot: StringBuffer = null)(implicit ctx: Context): Boolean = {

      /** Are we inside definition of `boundary`? */
      def accessWithin(boundary: Symbol) = {
        def test(implicit ctx: Context) =
          ctx.owner.isContainedIn(boundary) &&
            (!(this is JavaDefined) || // disregard package nesting for Java
               ctx.owner.enclosingPackageClass == boundary.enclosingPackageClass)
        try test
        catch {
          // It might be we are in a definition whose symbol is not defined at the
          // period where the test is made. Retry with FutureDefsOK. The reason
          // for not doing this outright is speed. We would like to avoid
          // creating a new context object each time we call accessWithin.
          // Note that the exception should be thrown only infrequently.
          case ex: NotDefinedHere => test(ctx.addMode(Mode.FutureDefsOK))
        }
      }

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
            s""" Access to protected $this not permitted because
               | enclosing ${ctx.owner.enclosingClass.showLocated} is not a subclass of
               | ${owner.showLocated} where target is defined""".stripMargin)
        else if (
          !(  isType // allow accesses to types from arbitrary subclasses fixes #4737
           || pre.baseTypeRef(cls).exists // ??? why not use derivesFrom ???
           || isConstructor
           || (owner is ModuleClass) // don't perform this check for static members
           ))
          fail(
            s""" Access to protected ${symbol.show} not permitted because
               | prefix type ${pre.widen.show} does not conform to
               | ${cls.showLocated} where the access takes place""".stripMargin)
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
             || (owner is ImplClass) // allow private local accesses to impl class members
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
    def membersNeedAsSeenFrom(pre: Type)(implicit ctx: Context) =
      !(  this.isTerm
       || this.isStaticOwner
       || ctx.erasedTypes
       || (pre eq NoPrefix) || (pre eq thisType)
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
     *   - labels
     *   - non-lazy valdefs
     */
    def isWeakOwner(implicit ctx: Context): Boolean =
      isPackageObject ||
      isTerm && !is(MethodOrLazy, butNot = Label) && !isLocalDummy

    //    def isOverridable: Boolean = !!! need to enforce that classes cannot be redefined
    def isSkolem: Boolean = name == nme.SKOLEM

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
    /** The class implementing this module, NoSymbol if not applicable. */
    final def moduleClass(implicit ctx: Context): Symbol = {
      def notFound = { println(s"missing module class for $name: $myInfo"); NoSymbol }
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
      else NoSymbol
    }

    /** The module implemented by this module class, NoSymbol if not applicable. */
    final def sourceModule(implicit ctx: Context): Symbol = myInfo match {
      case ClassInfo(_, _, _, _, selfType) if this is ModuleClass =>
        selfType match {
          case selfType: TermRef => selfType.symbol
          case selfType: Symbol => selfType.info.asInstanceOf[TermRef].symbol
        }
      case info: LazyType =>
        info.sourceModule
      case _ =>
        NoSymbol
    }

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
    final def ownersIterator(implicit ctx: Context) = new Iterator[Symbol] {
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

    /** The owner, skipping package objects, labels and non-lazy valdefs. */
    final def effectiveOwner(implicit ctx: Context) = owner.skipWeakOwner

    /** The class containing this denotation.
     *  If this denotation is already a class, return itself
     *  Definitions flagged with InSuperCall are treated specially.
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
        def newSkip = sym.is(InSuperCall) || sym.is(JavaStaticTerm)
        if (!sym.exists)
          NoSymbol
        else if (sym.isClass)
          if (skip) enclClass(sym.owner, newSkip) else sym
        else
          enclClass(sym.owner, skip || newSkip)
      }
      enclClass(symbol, false)
    }

    /** A symbol is effectively final if it cannot be overridden in a subclass */
    final def isEffectivelyFinal(implicit ctx: Context): Boolean =
      is(PrivateOrFinal) || !owner.isClass || owner.is(ModuleOrFinal) || owner.isAnonymousClass

    /** The class containing this denotation which has the given effective name. */
    final def enclosingClassNamed(name: Name)(implicit ctx: Context): Symbol = {
      val cls = enclosingClass
      if (cls.effectiveName == name || !cls.exists) cls else cls.owner.enclosingClassNamed(name)
    }

    /** The closest enclosing method containing this definition.
     *  A local dummy owner is mapped to the primary constructor of the class.
     */
    final def enclosingMethod(implicit ctx: Context): Symbol =
      if (this is (Method, butNot = Label)) symbol
      else if (this.isClass) primaryConstructor
      else if (this.exists) owner.enclosingMethod
      else NoSymbol

    /** The top-level class containing this denotation,
     *  except for a toplevel module, where its module class is returned.
     */
    final def topLevelClass(implicit ctx: Context): Symbol = {
      def topLevel(d: SymDenotation): Symbol = {
        if (d.isEffectiveRoot || (d is PackageClass) || (d.owner is PackageClass)) d.symbol
        else topLevel(d.owner)
      }
      val sym = topLevel(this)
      if (sym.isClass) sym else sym.moduleClass
    }

    /** The package class containing this denotation */
    final def enclosingPackageClass(implicit ctx: Context): Symbol =
      if (this is PackageClass) symbol else owner.enclosingPackageClass

    /** The module object with the same (term-) name as this class or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this module does not exist.
     */
    final def companionModule(implicit ctx: Context): Symbol = {
      if (this.flagsUNSAFE is Flags.Module) this.sourceModule
      else {
        val companionMethod = info.decls.denotsNamed(nme.COMPANION_MODULE_METHOD, selectPrivate).first
        if (companionMethod.exists)
          companionMethod.info.resultType.classSymbol.sourceModule
        else
          NoSymbol
      }
    }


    /** The class with the same (type-) name as this module or module class,
      *  and which is also defined in the same scope and compilation unit.
      *  NoSymbol if this class does not exist.
      */
    final def companionClass(implicit ctx: Context): Symbol = {
      val companionMethod = info.decls.denotsNamed(nme.COMPANION_CLASS_METHOD, selectPrivate).first

      if (companionMethod.exists)
        companionMethod.info.resultType.classSymbol
      else
        NoSymbol
    }

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
        owner.info.decl(name).suchThat(_.isCoDefinedWith(symbol)).symbol
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
        companionNamed(name)(ctx.outersIterator.dropWhile(_.scope eq ctx.scope).next)

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
    final def enclosingSubClass(implicit ctx: Context) =
      ctx.owner.ownersIterator.findSymbol(_.isSubClass(symbol))

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

    /** The symbol, in class `inClass`, that is overridden by this denotation. */
    final def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (!canMatchInheritedSymbols && (owner ne inClass)) NoSymbol
      else matchingDecl(inClass, owner.thisType)

    /** All symbols overriden by this denotation. */
    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.info)

    /** Returns all matching symbols defined in parents of the selftype. */
    final def extendedOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      if (!canMatchInheritedSymbols) Iterator.empty
      else overriddenFromType(owner.asClass.classInfo.selfType)

    private def overriddenFromType(tp: Type)(implicit ctx: Context): Iterator[Symbol] =
      tp.baseClasses.tail.iterator map overriddenSymbol filter (_.exists)

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
      def loop(bcs: List[ClassSymbol]): Symbol = bcs match {
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
    final def isIncompleteIn(base: Symbol)(implicit ctx: Context): Boolean =
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

    // ----- type-related ------------------------------------------------

    /** The type parameters of a class symbol, Nil for all other symbols */
    def typeParams(implicit ctx: Context): List[TypeSymbol] = Nil

    /** The named type parameters declared or inherited by this symbol */
    def namedTypeParams(implicit ctx: Context): Set[TypeSymbol] = Set()

    /** The type This(cls), where cls is this class, NoPrefix for all other symbols */
    def thisType(implicit ctx: Context): Type = NoPrefix

    override def typeRef(implicit ctx: Context): TypeRef =
      TypeRef(owner.thisType, name.asTypeName, this)

    override def termRef(implicit ctx: Context): TermRef =
      TermRef(owner.thisType, name.asTermName, this)

    override def valRef(implicit ctx: Context): TermRef =
      TermRef.withSigAndDenot(owner.thisType, name.asTermName, Signature.NotAMethod, this)

    override def termRefWithSig(implicit ctx: Context): TermRef =
      TermRef.withSigAndDenot(owner.thisType, name.asTermName, signature, this)

    def nonMemberTermRef(implicit ctx: Context): TermRef =
      TermRef.withFixedSym(owner.thisType, name.asTermName, symbol.asTerm)

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

    override def toString = {
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

    def debugString = toString + "#" + symbol.id // !!! DEBUG

    def hasSkolems(tp: Type): Boolean = tp match {
      case tp: SkolemType => true
      case tp: NamedType => hasSkolems(tp.prefix)
      case tp: RefinedType => hasSkolems(tp.parent) || hasSkolems(tp.refinedInfo)
      case tp: RecType => hasSkolems(tp.parent)
      case tp: PolyType => tp.paramBounds.exists(hasSkolems) || hasSkolems(tp.resType)
      case tp: MethodType => tp.paramTypes.exists(hasSkolems) || hasSkolems(tp.resType)
      case tp: ExprType => hasSkolems(tp.resType)
      case tp: AndOrType => hasSkolems(tp.tp1) || hasSkolems(tp.tp2)
      case tp: TypeBounds => hasSkolems(tp.lo) || hasSkolems(tp.hi)
      case tp: AnnotatedType => hasSkolems(tp.tpe)
      case tp: TypeVar => hasSkolems(tp.inst)
      case _ => false
    }

    def assertNoSkolems(tp: Type) =
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
      annotations: List[Annotation] = null)(implicit ctx: Context) =
    { // simulate default parameters, while also passing implicit context ctx to the default values
      val initFlags1 = (if (initFlags != UndefinedFlags) initFlags else this.flags) &~ Frozen
      val info1 = if (info != null) info else this.info
      val privateWithin1 = if (privateWithin != null) privateWithin else this.privateWithin
      val annotations1 = if (annotations != null) annotations else this.annotations
      val d = ctx.SymDenotation(symbol, owner, name, initFlags1, info1, privateWithin1)
      d.annotations = annotations1
      d
    }

    /** Install this denotation as the result of the given denotation transformer. */
    override def installAfter(phase: DenotTransformer)(implicit ctx: Context): Unit =
      super.installAfter(phase)

    /** Apply a transformation `f` to all denotations in this group that start at or after
     *  given phase. Denotations are replaced while keeping the same validity periods.
     */
    override def transformAfter(phase: DenotTransformer, f: SymDenotation => SymDenotation)(implicit ctx: Context): Unit =
      super.transformAfter(phase, f)

    /** If denotation is private, remove the Private flag and expand the name if necessary */
    def ensureNotPrivate(implicit ctx: Context) =
      if (is(Private))
        copySymDenotation(
          name = expandedName,
          initFlags = this.flags &~ Private | ExpandedName)
      else this
  }

  /** The contents of a class definition during a period
   */
  class ClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol,
    initRunId: RunId)
    extends SymDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin) {

    import util.LRUCache

    // ----- denotation fields and accessors ------------------------------

    if (initFlags is (Module, butNot = Package)) assert(name.isModuleClassName, s"module naming inconsistency: $name")

    /** The symbol asserted to have type ClassSymbol */
    def classSymbol: ClassSymbol = symbol.asInstanceOf[ClassSymbol]

    /** The info asserted to have type ClassInfo */
    def classInfo(implicit ctx: Context): ClassInfo = info.asInstanceOf[ClassInfo]

    /** TODO: Document why caches are supposedly safe to use */
    private[this] var myTypeParams: List[TypeSymbol] = _

    private[this] var myNamedTypeParams: Set[TypeSymbol] = _

    /** The type parameters of this class */
    override final def typeParams(implicit ctx: Context): List[TypeSymbol] = {
      def computeTypeParams = {
        if (ctx.erasedTypes || is(Module)) Nil // fast return for modules to avoid scanning package decls
        else if (this ne initial) initial.asSymDenotation.typeParams
        else unforcedDecls.filter(sym =>
          (sym is TypeParam) && sym.owner == symbol).asInstanceOf[List[TypeSymbol]]
      }
      if (myTypeParams == null) myTypeParams = computeTypeParams
      myTypeParams
    }

    /** The named type parameters declared or inherited by this class */
    override final def namedTypeParams(implicit ctx: Context): Set[TypeSymbol] = {
      def computeNamedTypeParams: Set[TypeSymbol] =
        if (ctx.erasedTypes || is(Module)) Set() // fast return for modules to avoid scanning package decls
        else memberNames(abstractTypeNameFilter).map(name =>
          info.member(name).symbol.asType).filter(_.is(TypeParam, butNot = ExpandedName)).toSet
      if (myNamedTypeParams == null) myNamedTypeParams = computeNamedTypeParams
      myNamedTypeParams
    }

    override protected[dotc] final def info_=(tp: Type) = {
      super.info_=(tp)
      myTypeParams = null // changing the info might change decls, and with it typeParams
    }

    /** The denotations of all parents in this class. */
    def classParents(implicit ctx: Context): List[TypeRef] = info match {
      case classInfo: ClassInfo => classInfo.classParents
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

    /** The denotation is fully completed: all attributes are fully defined.
     *  ClassDenotations compiled from source are first completed, then fully completed.
     *  Packages are never fully completed since members can be added at any time.
     *  @see Namer#ClassCompleter
     */
    private def isFullyCompleted(implicit ctx: Context): Boolean = {
      def isFullyCompletedRef(tp: TypeRef) = tp.denot match {
        case d: ClassDenotation => d.isFullyCompleted
        case _ => false
      }
      def testFullyCompleted =
        if (classParents.isEmpty) !is(Package) && symbol.eq(defn.AnyClass)
        else classParents.forall(isFullyCompletedRef)
      flagsUNSAFE.is(FullyCompleted) ||
        isCompleted && testFullyCompleted && { setFlag(FullyCompleted); true }
    }

    // ------ syncing inheritance-related info -----------------------------

    private var firstRunId: RunId = initRunId

    /** invalidate caches influenced by parent classes if one of the parents
     *  is younger than the denotation itself.
     */
    override def syncWithParents(implicit ctx: Context): SingleDenotation = {
      def isYounger(tref: TypeRef) = tref.symbol.denot match {
        case denot: ClassDenotation =>
          if (denot.validFor.runId < ctx.runId) denot.current // syncs with its parents in turn
          val result = denot.firstRunId > this.firstRunId
          if (result) incremental.println(s"$denot is younger than $this")
          result
        case _ => false
      }
      val parentIsYounger = (firstRunId < ctx.runId) && {
        infoOrCompleter match {
          case cinfo: ClassInfo => cinfo.classParents exists isYounger
          case _ => false
        }
      }
      if (parentIsYounger) {
        incremental.println(s"parents of $this are invalid; symbol id = ${symbol.id}, copying ...\n")
        invalidateInheritedInfo()
      }
      firstRunId = ctx.runId
      this
    }

    /** Invalidate all caches and fields that depend on base classes and their contents */
    override def invalidateInheritedInfo(): Unit = {
      myBaseClasses = null
      mySuperClassBits = null
      myMemberFingerPrint = FingerPrint.unknown
      myMemberCache = null
      myMemberCachePeriod = Nowhere
      memberNamesCache = SimpleMap.Empty
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

    private def computeThisType(implicit ctx: Context): Type =
      ThisType.raw(
        TypeRef(if (this is Package) NoPrefix else owner.thisType, symbol.asType))
/*      else {
        val pre = owner.thisType
        if (this is Module)
          if (isMissing(pre)) TermRef(pre, sourceModule.asTerm)
          else TermRef.withSig(pre, name.sourceModuleName, Signature.NotAMethod)
        else ThisType.raw(TypeRef(pre, symbol.asType))
      }
*/
    private[this] var myTypeRef: TypeRef = null

    override def typeRef(implicit ctx: Context): TypeRef = {
      if (myTypeRef == null) myTypeRef = super.typeRef
      myTypeRef
    }

    private[this] var myBaseClasses: List[ClassSymbol] = null
    private[this] var mySuperClassBits: BitSet = null

    /** Invalidate baseTypeRefCache, baseClasses and superClassBits on new run */
    private def checkBasesUpToDate()(implicit ctx: Context) =
      if (baseTypeRefValid != ctx.runId) {
        baseTypeRefCache = new java.util.HashMap[CachedType, Type]
        myBaseClasses = null
        mySuperClassBits = null
        baseTypeRefValid = ctx.runId
      }

    private def computeBases(implicit ctx: Context): (List[ClassSymbol], BitSet) = {
      if (myBaseClasses eq Nil) throw CyclicReference(this)
      myBaseClasses = Nil
      val seen = new mutable.BitSet
      val locked = new mutable.BitSet
      def addBaseClasses(bcs: List[ClassSymbol], to: List[ClassSymbol])
          : List[ClassSymbol] = bcs match {
        case bc :: bcs1 =>
          val bcs1added = addBaseClasses(bcs1, to)
          val id = bc.superId
          if (seen contains id) bcs1added
          else {
            seen += id
            bc :: bcs1added
          }
        case nil =>
          to
      }
      def addParentBaseClasses(ps: List[Type], to: List[ClassSymbol]): List[ClassSymbol] = ps match {
        case p :: ps1 =>
          addParentBaseClasses(ps1, addBaseClasses(p.baseClasses, to))
        case nil =>
          to
      }
      val bcs = classSymbol :: addParentBaseClasses(classParents, Nil)
      val scbits = seen.toImmutable
      if (isFullyCompleted) {
        myBaseClasses = bcs
        mySuperClassBits = scbits
      }
      else myBaseClasses = null
      (bcs, scbits)
    }

    /** A bitset that contains the superId's of all base classes */
    private def superClassBits(implicit ctx: Context): BitSet =
      if (classParents.isEmpty) BitSet() // can happen when called too early in Namers
      else {
        checkBasesUpToDate()
        if (mySuperClassBits != null) mySuperClassBits else computeBases._2
      }

    /** The base classes of this class in linearization order,
     *  with the class itself as first element.
     */
    def baseClasses(implicit ctx: Context): List[ClassSymbol] =
      if (classParents.isEmpty) classSymbol :: Nil // can happen when called too early in Namers
      else {
        checkBasesUpToDate()
        if (myBaseClasses != null) myBaseClasses else computeBases._1
      }

    final override def derivesFrom(base: Symbol)(implicit ctx: Context): Boolean =
      !isAbsent &&
      base.isClass &&
      (  (symbol eq base)
      || (superClassBits contains base.superId)
      || (this is Erroneous)
      || (base is Erroneous)
      )

    final override def isSubClass(base: Symbol)(implicit ctx: Context) =
      derivesFrom(base) ||
        base.isClass && (
          (symbol eq defn.NothingClass) ||
            (symbol eq defn.NullClass) && (base ne defn.NothingClass))

    final override def typeParamCreationFlags = ClassTypeParamCreationFlags

    private[this] var myMemberFingerPrint: FingerPrint = FingerPrint.unknown

    private def computeMemberFingerPrint(implicit ctx: Context): FingerPrint = {
      var fp = FingerPrint()
      var e = info.decls.lastEntry
      while (e != null) {
        fp.include(e.name)
        e = e.prev
      }
      var ps = classParents
      while (ps.nonEmpty) {
        val parent = ps.head.typeSymbol
        parent.denot match {
          case parentDenot: ClassDenotation =>
            fp.include(parentDenot.memberFingerPrint)
            if (parentDenot.isFullyCompleted) parentDenot.setFlag(Frozen)
          case _ =>
        }
        ps = ps.tail
      }
      fp
    }

    /** A bloom filter for the names of all members in this class.
     *  Makes sense only for parent classes, and should definitely
     *  not be used for package classes because cache never
     *  gets invalidated.
     */
    def memberFingerPrint(implicit ctx: Context): FingerPrint =
      if (myMemberFingerPrint != FingerPrint.unknown) myMemberFingerPrint
      else {
        val fp = computeMemberFingerPrint
        if (isFullyCompleted) myMemberFingerPrint = fp
        fp
      }

    private[this] var myMemberCache: LRUCache[Name, PreDenotation] = null
    private[this] var myMemberCachePeriod: Period = Nowhere

    private def memberCache(implicit ctx: Context): LRUCache[Name, PreDenotation] = {
      if (myMemberCachePeriod != ctx.period) {
        myMemberCache = new LRUCache
        myMemberCachePeriod = ctx.period
      }
      myMemberCache
    }

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
      if (this is PackageClass) {
        val entry = mscope.lookupEntry(sym.name)
        if (entry != null) {
          if (entry.sym == sym) return
          mscope.unlink(entry)
          entry.sym.denot = sym.denot // to avoid stale symbols
        }
      }
      enterNoReplace(sym, mscope)
      val nxt = this.nextInRun
      if (nxt.validFor.code > this.validFor.code) {
        this.nextInRun.asSymDenotation.asClass.enter(sym)
      }
    }

    /** Enter a symbol in given `scope` without potentially replacing the old copy. */
    def enterNoReplace(sym: Symbol, scope: MutableScope)(implicit ctx: Context): Unit = {
      require((sym.denot.flagsUNSAFE is Private) ||  !(this is Frozen) || (scope ne this.unforcedDecls) || sym.hasAnnotation(defn.ScalaStaticAnnot))
      scope.enter(sym)

      if (myMemberFingerPrint != FingerPrint.unknown)
        myMemberFingerPrint.include(sym.name)
      if (myMemberCache != null)
        myMemberCache invalidate sym.name
    }

    /** Replace symbol `prev` (if defined in current class) by symbol `replacement`.
     *  If `prev` is not defined in current class, do nothing.
     *  @pre `prev` and `replacement` have the same name.
     */
    def replace(prev: Symbol, replacement: Symbol)(implicit ctx: Context): Unit = {
      require(!(this is Frozen))
      unforcedDecls.openForMutations.replace(prev, replacement)
      if (myMemberCache != null)
        myMemberCache invalidate replacement.name
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      info.decls.openForMutations.unlink(sym)
      myMemberFingerPrint = FingerPrint.unknown
      if (myMemberCache != null) myMemberCache invalidate sym.name
    }

    /** Make sure the type parameters of this class are `tparams`, reorder definitions
     *  in scope if necessary.
     *  @pre  All type parameters in `tparams` are entered in class scope `info.decls`.
     */
    def updateTypeParams(tparams: List[Symbol])(implicit ctx: Context): Unit =
      if (!typeParams.corresponds(tparams)(_.name == _.name)) {
        val decls = info.decls
        val decls1 = newScope
        for (tparam <- tparams) decls1.enter(decls.lookup(tparam.name))
        for (sym <- decls) if (!typeParams.contains(sym)) decls1.enter(sym)
        info = classInfo.derivedClassInfo(decls = decls1)
        myTypeParams = null
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
    final def nonPrivateMembersNamed(name: Name, inherited: Boolean = false)(implicit ctx: Context): PreDenotation = {
      Stats.record("nonPrivateMembersNamed")
      if (Config.cacheMembersNamed) {
        var denots: PreDenotation = memberCache lookup name
        if (denots == null) {
          denots = computeNPMembersNamed(name, inherited)
          if (isFullyCompleted) memberCache.enter(name, denots)
        } else if (Config.checkCacheMembersNamed) {
          val denots1 = computeNPMembersNamed(name, inherited)
          assert(denots.exists == denots1.exists, s"cache inconsistency: cached: $denots, computed $denots1, name = $name, owner = $this")
        }
        denots
      } else computeNPMembersNamed(name, inherited)
    }

    private[core] def computeNPMembersNamed(name: Name, inherited: Boolean)(implicit ctx: Context): PreDenotation = /*>|>*/ Stats.track("computeNPMembersNamed") /*<|<*/ {
      if (!inherited ||
          !Config.useFingerPrints ||
          (memberFingerPrint contains name)) {
        Stats.record("computeNPMembersNamed after fingerprint")
        ensureCompleted()
        val ownDenots = info.decls.denotsNamed(name, selectNonPrivate)
        if (debugTrace)  // DEBUG
          println(s"$this.member($name), ownDenots = $ownDenots")
        def collect(denots: PreDenotation, parents: List[TypeRef]): PreDenotation = parents match {
          case p :: ps =>
            val denots1 = collect(denots, ps)
            p.symbol.denot match {
              case parentd: ClassDenotation =>
                denots1 union
                  parentd.nonPrivateMembersNamed(name, inherited = true)
                    .mapInherited(ownDenots, denots1, thisType)
              case _ =>
                denots1
            }
          case nil =>
            denots
        }
        if (name.isConstructorName) ownDenots
        else collect(ownDenots, classParents)
      } else NoDenotation
    }

    override final def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = {
      val raw = if (excluded is Private) nonPrivateMembersNamed(name) else membersNamed(name)
      raw.filterExcluded(excluded).asSeenFrom(pre).toDenot(pre)
    }

    private[this] var baseTypeRefCache: java.util.HashMap[CachedType, Type] = null
    private[this] var baseTypeRefValid: RunId = NoRunId

    /** Compute tp.baseTypeRef(this) */
    final def baseTypeRefOf(tp: Type)(implicit ctx: Context): Type = {

      def foldGlb(bt: Type, ps: List[Type]): Type = ps match {
        case p :: ps1 => foldGlb(bt & baseTypeRefOf(p), ps1)
        case _ => bt
      }

      def inCache(tp: Type) = baseTypeRefCache.containsKey(tp)

      /** We cannot cache:
       *  - type variables which are uninstantiated or whose instances can
       *    change, depending on typerstate.
       *  - types where the underlying type is an ErasedValueType, because
       *    this underlying type will change after ElimErasedValueType,
       *    and this changes subtyping relations. As a shortcut, we do not
       *    cache ErasedValueType at all.
       */
      def isCachable(tp: Type): Boolean = tp match {
        case _: TypeErasure.ErasedValueType => false
        case tp: TypeVar => tp.inst.exists && inCache(tp.inst)
        case tp: TypeProxy => inCache(tp.underlying)
        case tp: AndOrType => inCache(tp.tp1) && inCache(tp.tp2)
        case _ => true
      }

      def computeBaseTypeRefOf(tp: Type): Type = {
        Stats.record("computeBaseTypeOf")
        if (symbol.isStatic && tp.derivesFrom(symbol))
          symbol.typeRef
        else tp match {
          case tp: TypeRef =>
            val subcls = tp.symbol
            if (subcls eq symbol)
              tp
            else subcls.denot match {
              case cdenot: ClassDenotation =>
                if (cdenot.superClassBits contains symbol.superId) foldGlb(NoType, tp.parents)
                else NoType
              case _ =>
                baseTypeRefOf(tp.underlying)
            }
          case tp: TypeProxy =>
            baseTypeRefOf(tp.underlying)
          case AndType(tp1, tp2) =>
            baseTypeRefOf(tp1) & baseTypeRefOf(tp2)
          case OrType(tp1, tp2) =>
            baseTypeRefOf(tp1) | baseTypeRefOf(tp2)
          case JavaArrayType(_) if symbol == defn.ObjectClass =>
            this.typeRef
          case _ =>
            NoType
        }
      }

      /*>|>*/ ctx.debugTraceIndented(s"$tp.baseTypeRef($this)") /*<|<*/ {
        tp match {
          case tp: CachedType =>
            checkBasesUpToDate()
            var basetp = baseTypeRefCache get tp
            if (basetp == null) {
              baseTypeRefCache.put(tp, NoPrefix)
              basetp = computeBaseTypeRefOf(tp)
              if (isCachable(tp)) baseTypeRefCache.put(tp, basetp)
              else baseTypeRefCache.remove(tp)
            } else if (basetp == NoPrefix) {
              baseTypeRefCache.put(tp, null)
              throw CyclicReference(this)
            }
            basetp
          case _ =>
            computeBaseTypeRefOf(tp)
        }
      }
    }

    private[this] var memberNamesCache: SimpleMap[NameFilter, Set[Name]] = SimpleMap.Empty

    def memberNames(keepOnly: NameFilter)(implicit ctx: Context): Set[Name] = {
      def computeMemberNames: Set[Name] = {
        var names = Set[Name]()
        def maybeAdd(name: Name) = if (keepOnly(thisType, name)) names += name
        for (p <- classParents)
          for (name <- p.memberNames(keepOnly, thisType)) maybeAdd(name)
        val ownSyms =
          if (keepOnly == implicitFilter)
            if (this is Package) Iterator.empty
            else info.decls.iterator filter (_ is Implicit)
          else info.decls.iterator
        for (sym <- ownSyms) maybeAdd(sym.name)
        names
      }
      if ((this is PackageClass) || !Config.cacheMemberNames)
        computeMemberNames // don't cache package member names; they might change
      else {
        val cached = memberNamesCache(keepOnly)
        if (cached != null) cached
        else {
          val names = computeMemberNames
          if (isFullyCompleted) {
            setFlag(Frozen)
            memberNamesCache = memberNamesCache.updated(keepOnly, names)
          }
          names
        }
      }
    }

    private[this] var fullNameCache: SimpleMap[String, Name] = SimpleMap.Empty
    override final def fullNameSeparated(separator: String)(implicit ctx: Context): Name = {
      val cached = fullNameCache(separator)
      if (cached != null) cached
      else {
        val fn = super.fullNameSeparated(separator)
        fullNameCache = fullNameCache.updated(separator, fn)
        fn
      }
    }

    // to avoid overloading ambiguities
    override def fullName(implicit ctx: Context): Name = super.fullName

    override def primaryConstructor(implicit ctx: Context): Symbol = {
      def constrNamed(cname: TermName) = info.decls.denotsNamed(cname).last.symbol
        // denotsNamed returns Symbols in reverse order of occurrence
      if (this.is(ImplClass)) constrNamed(nme.TRAIT_CONSTRUCTOR) // ignore normal constructor
      else
        constrNamed(nme.CONSTRUCTOR).orElse(constrNamed(nme.TRAIT_CONSTRUCTOR))
    }

    /** The parameter accessors of this class. Term and type accessors,
     *  getters and setters are all returned int his list
     */
    def paramAccessors(implicit ctx: Context): List[Symbol] =
      unforcedDecls.filter(_ is ParamAccessor).toList

    /** If this class has the same `decls` scope reference in `phase` and
     *  `phase.next`, install a new denotation with a cloned scope in `phase.next`.
     */
    def ensureFreshScopeAfter(phase: DenotTransformer)(implicit ctx: Context): Unit =
      if (ctx.phaseId != phase.next.id) ensureFreshScopeAfter(phase)(ctx.withPhase(phase.next))
      else {
        val prevCtx = ctx.withPhase(phase)
        val ClassInfo(pre, _, ps, decls, selfInfo) = classInfo
        if (classInfo(prevCtx).decls eq decls)
          copySymDenotation(info = ClassInfo(pre, classSymbol, ps, decls.cloneScope, selfInfo))
            .installAfter(phase)
      }
  }

  /** The denotation of a package class.
   *  It overrides ClassDenotation to take account of package objects when looking for members
   */
  class PackageClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol,
    initRunId: RunId)
    extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin, initRunId) {

    private[this] var packageObjCache: SymDenotation = _
    private[this] var packageObjRunId: RunId = NoRunId

    /** The package object in this class, of one exists */
    def packageObj(implicit ctx: Context): SymDenotation = {
      if (packageObjRunId != ctx.runId) {
        packageObjRunId = ctx.runId
        packageObjCache = NoDenotation // break cycle in case we are looking for package object itself
        packageObjCache = findMember(nme.PACKAGE, thisType, EmptyFlags).asSymDenotation
      }
      packageObjCache
    }

    /** Look first for members in package; if none are found look in package object */
    override def computeNPMembersNamed(name: Name, inherited: Boolean)(implicit ctx: Context): PreDenotation = {
      val denots = super.computeNPMembersNamed(name, inherited)
      if (denots.exists) denots
      else packageObj.moduleClass.denot match {
        case pcls: ClassDenotation => pcls.computeNPMembersNamed(name, inherited)
        case _ => denots
      }
    }

    /** The union of the member names of the package and the package object */
    override def memberNames(keepOnly: NameFilter)(implicit ctx: Context): Set[Name] = {
      val ownNames = super.memberNames(keepOnly)
      packageObj.moduleClass.denot match {
        case pcls: ClassDenotation => ownNames union pcls.memberNames(keepOnly)
        case _ => ownNames
      }
    }
  }

  class NoDenotation extends SymDenotation(
    NoSymbol, NoSymbol, "<none>".toTermName, Permanent, NoType) {
    override def exists = false
    override def isTerm = false
    override def isType = false
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def computeAsSeenFrom(pre: Type)(implicit ctx: Context): SingleDenotation = this
    validFor = Period.allInRun(NoRunId) // will be brought forward automatically
  }

  @sharable val NoDenotation = new NoDenotation
  @sharable val NotDefinedHereDenotation = new NoDenotation

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

    def apply(sym: Symbol) = this
    def apply(module: TermSymbol, modcls: ClassSymbol) = this

    private var myDecls: Scope = EmptyScope
    private var mySourceModuleFn: Context => Symbol = NoSymbolFn
    private var myModuleClassFn: Context => Symbol = NoSymbolFn

    /** A proxy to this lazy type that keeps the complete operation
     *  but provides fresh slots for scope/sourceModule/moduleClass
     */
    def proxy: LazyType = new LazyType {
      override def complete(denot: SymDenotation)(implicit ctx: Context) = self.complete(denot)
    }

    def decls: Scope = myDecls
    def sourceModule(implicit ctx: Context): Symbol = mySourceModuleFn(ctx)
    def moduleClass(implicit ctx: Context): Symbol = myModuleClassFn(ctx)

    def withDecls(decls: Scope): this.type = { myDecls = decls; this }
    def withSourceModule(sourceModuleFn: Context => Symbol): this.type = { mySourceModuleFn = sourceModuleFn; this }
    def withModuleClass(moduleClassFn: Context => Symbol): this.type = { myModuleClassFn = moduleClassFn; this }
  }

  /** A subclass of LazyTypes where type parameters can be completed independently of
   *  the info.
   */
  abstract class TypeParamsCompleter extends LazyType {
    /** The type parameters computed by the completer before completion has finished */
    def completerTypeParams(sym: Symbol): List[TypeSymbol]
  }

  val NoSymbolFn = (ctx: Context) => NoSymbol

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
    override def moduleClass(implicit ctx: Context) = _moduleClass
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

    def initializeToDefaults(denot: SymDenotation)(implicit ctx: Context) = {
      denot.info = denot match {
        case denot: ClassDenotation =>
          ClassInfo(denot.owner.thisType, denot.classSymbol, Nil, EmptyScope)
        case _ =>
          ErrorType
      }
      denot.privateWithin = NoSymbol
    }

    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
      val sym = denot.symbol
      val file = sym.associatedFile
      val (location, src) =
        if (file != null) (s" in $file", file.toString)
        else ("", "the signature")
      val name = ctx.fresh.setSetting(ctx.settings.debugNames, true).nameString(denot.name)
      ctx.error(
        s"""|bad symbolic reference. A signature$location
            |refers to $name in ${denot.owner.showKind} ${denot.owner.showFullName} which is not available.
            |It may be completely missing from the current classpath, or the version on
            |the classpath might be incompatible with the version used when compiling $src.""".stripMargin)
      if (ctx.debug) throw new Error()
      initializeToDefaults(denot)
    }
  }

  // ---- Fingerprints -----------------------------------------------------

  /** A fingerprint is a bitset that acts as a bloom filter for sets
   *  of names.
   */
  class FingerPrint(val bits: Array[Long]) extends AnyVal {
    import FingerPrint._

    /** Include some bits of name's hashcode in set */
    def include(name: Name): Unit = {
      val hash = name.hashCode & Mask
      bits(hash >> WordSizeLog) |= (1L << hash)
    }

    /** Include all bits of `that` fingerprint in set */
    def include(that: FingerPrint): Unit =
      for (i <- 0 until NumWords) bits(i) |= that.bits(i)

    /** Does set contain hash bits of given name? */
    def contains(name: Name): Boolean = {
      val hash = name.hashCode & Mask
      (bits(hash >> WordSizeLog) & (1L << hash)) != 0
    }
  }

  object FingerPrint {
    def apply() = new FingerPrint(new Array[Long](NumWords))
    val unknown = new FingerPrint(null)
    private final val WordSizeLog = 6
    private final val NumWords = 32
    private final val NumBits = NumWords << WordSizeLog
    private final val Mask = NumBits - 1
  }

  private val AccessorOrLabel = Accessor | Label

  @sharable private var indent = 0 // for completions printing
}
