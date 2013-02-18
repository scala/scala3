package dotty.tools.dotc
package core

import Periods._, Contexts._, Symbols._, Denotations._, Names._, Annotations._
import Types._, Flags._, Decorators._, Transformers._, StdNames._, Scopes._
import NameOps._
import Scopes.Scope
import collection.mutable
import collection.immutable.BitSet
import scala.reflect.io.AbstractFile
import Decorators.SymbolIteratorDecorator
import annotation.tailrec

object SymDenotations {

  /** A denotation represents the contents of a definition
   *  during a period.
   */
  abstract class SymDenotation(initFlags: FlagSet) extends SingleDenotation {

// ----- denotation fields and accessors ------------------------------

    /** The denoting symbol */
    def symbol: Symbol

    /** The owner. Owners form chains that always end in
     *  defn.RootClass
     */
    def owner: Symbol

    /** The name */
    def name: Name

    /** The privateWithin boundary, NoSymbol if no boundary is given. */
    def privateWithin: Symbol

    /** The type info.
     *  The info an instance of TypeType iff this is a type denotation
     */
    def info: Type

    /** The name with which the denoting symbol was created */
    def originalName =
      if (flags is ExpandedName) initial.asSymDenotation.name else name

    /** The encoded full path name of this denotation, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Drops package objects.
     */
    def fullName(separator: Char)(implicit ctx: Context): Name =
      if (this == NoSymbol || owner == NoSymbol || owner.isEffectiveRoot) name
      else (effectiveOwner.enclosingClass.fullName(separator) :+ separator) ++ name

    /** `fullName` where `.' is the separator character */
    def fullName(implicit ctx: Context): Name = fullName('.')

    /** The source or class file from which this denotation was generated, null if not applicable. */
    def associatedFile(implicit ctx: Context): AbstractFile = topLevelClass.associatedFile

    /** The class file from which this denotation was generated, null if not applicable. */
    final def binaryFile(implicit ctx: Context): AbstractFile = pickFile(associatedFile, classFile = true)

    /** The source file from which this denotation was generated, null if not applicable. */
    final def sourceFile(implicit ctx: Context): AbstractFile = pickFile(associatedFile, classFile = false)

   /** Desire to re-use the field in ClassSymbol which stores the source
     *  file to also store the classfile, but without changing the behavior
     *  of sourceFile (which is expected at least in the IDE only to
     *  return actual source code.) So sourceFile has classfiles filtered out.
     */
    private def pickFile(file: AbstractFile, classFile: Boolean): AbstractFile =
      if ((file eq null) || classFile != (file.path endsWith ".class")) null else file

    private[this] var _flags: FlagSet = initFlags

    /** The flag set */
    def flags: FlagSet = { ensureLoaded(); _flags }

    /** Update the flag set */
    private[core] def flags_=(flags: FlagSet): Unit = { _flags = flags }

    /** Set given flags(s) of this denotation */
    def setFlag(flags: FlagSet): Unit = { _flags |= flags }

     /** UnsSet given flags(s) of this denotation */
    def resetFlag(flags: FlagSet): Unit = { _flags &~= flags }

    private[this] var _annotations: List[Annotation] = Nil

    /** The annotations of this denotation */
    def annotations: List[Annotation] = { ensureCompleted(); _annotations }

    /** Update the annotations of this denotation */
    private[core] def annotations_=(annots: List[Annotation]): Unit = { _annotations = annots }

    /** Does this denotation have an annotation matching the given class symbol? */
    def hasAnnotation(cls: Symbol)(implicit ctx: Context) = dropOtherAnnotations(annotations, cls).nonEmpty

    /** Add given annotation to the annotations of this denotation */
    final def addAnnotation(annot: Annotation): Unit = annotations =
      annot :: annotations

    @tailrec
    private def dropOtherAnnotations(anns: List[Annotation], cls: Symbol)(implicit ctx: Context): List[Annotation] = anns match {
      case ann :: rest => if (ann matches cls) anns else dropOtherAnnotations(rest, cls)
      case Nil => Nil
    }

// ----- completion ------------------------------

    // The following 4 members are overridden by instances of isLazy

    /** The denotation is loaded: flags and privateWithin are fully defined. */
    def isLoaded = true

    /** The denotation is completed: all attributes are fully defined */
    def isCompleted = true

    /** Try to load denotation. May throw `CyclicReference`. */
    protected[core] def tryLoad(): Unit = unsupported("tryLoad")

    /** Try to complete denotation. May throw `CyclicReference`. */
    protected[core] def tryComplete(): Unit = unsupported("tryComplete")

    /** Make sure denotation is loaded */
    final def ensureLoaded() = if (!isLoaded) tryLoad()

    /** Make sure denotation is completed */
    final def ensureCompleted() = if (!isCompleted) tryComplete()

// ----- tests -------------------------------------------------

    /** Is this denotation a type? */
    override def isType: Boolean = name.isTypeName

    /** Is this denotation a class? */
    final def isClass: Boolean = symbol.isInstanceOf[ClassSymbol]

    /** Cast to class denotation */
    final def asClass: ClassDenotation = asInstanceOf[ClassDenotation]

    /** Special case tests for flags that are known a-priori and do not need loading
     *  flags.
     */
    final def isModule = _flags is Module
    final def isModuleVal = _flags is ModuleVal
    final def isModuleClass = _flags is ModuleClass
    final def isPackage = _flags is Package
    final def isPackageVal = _flags is PackageVal
    final def isPackageClass = _flags is PackageClass

    /** is this denotation a method? */
    //def isMethod: Boolean = false

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    /** The denotation refers to an existing definition.
     *  @return `false` if denotation is either `NoDenotation` or it
     *                  refers to a toplevel class or object that has no
     *                  definition in the source or classfile from which it is loaded.
     */
    def exists(implicit ctx: Context) = true

    /** Is this symbol the root class or its companion object? */
    def isRoot: Boolean = name.toTermName == nme.ROOT

    /** Is this symbol the empty package class or its companion object? */
    def isEmptyPackage(implicit ctx: Context): Boolean = name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
    def isEffectiveRoot(implicit ctx: Context) = isRoot || isEmptyPackage

    /** Is this symbol an anonymous class? */
    def isAnonymousClass(implicit ctx: Context): Boolean = initial.asSymDenotation.name startsWith tpnme.ANON_CLASS

    /** Is this symbol an abstract type? */
    def isAbstractType = isType && info.isRealTypeBounds

    /** Is this definition contained in `boundary`?
     *  Same as `ownersIterator contains boundary` but more efficient.
     */
    final def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean = {
      def recur(sym: Symbol): Boolean =
        if (sym eq boundary) true
        else if (sym eq NoSymbol) false
        else if (sym.isPackageClass && !boundary.isPackageClass) false
        else recur(sym.owner)
      recur(symbol)
    }

    /** Is this denotation static (i.e. with no outer instance)? */
    def isStatic(implicit ctx: Context) = (this is Static) || owner.isStaticOwner

    /** Is this a package class or module class that defines static symbols? */
    final def isStaticOwner(implicit ctx: Context): Boolean =
      isPackageClass || isModuleClass && isStatic

    /** Is this denotation defined in the same scope and compilation unit as that symbol? */
    def isCoDefinedWith(that: Symbol)(implicit ctx: Context) =
      (this.effectiveOwner == that.effectiveOwner) &&
      (  !(this.effectiveOwner.isPackageClass)
      || (this.sourceFile == null)
      || (that.sourceFile == null)
      || (this.sourceFile.path == that.sourceFile.path)  // Cheap possibly wrong check, then expensive normalization
      || (this.sourceFile.canonicalPath == that.sourceFile.canonicalPath)
      )

    /** Is this a denotation of a stable term? */
    final def isStable(implicit ctx: Context) = !(
      isTerm &&
      this.is(UnstableValue, butNot = Stable) ||
      info.isVolatile && !hasAnnotation(defn.uncheckedStableClass)
    )

    /** Is this a subclass of the given class `base`? */
    def isSubClass(base: Symbol)(implicit ctx: Context) = false

    /** Is this a user defined "def" method? Excluded are accessors and stable values */
    def isSourceMethod = this is (Method, butNot = Accessor)

    /** Is this NOT a user-defined "def" method that takes parameters? */
    def isParameterless(implicit ctx: Context) =
      !isSourceMethod || info.paramTypess.isEmpty

    /** Is this a setter? */
    def isGetter = (this is Accessor) && !originalName.isSetterName

    /** Is this a setter? */
    def isSetter = (this is Accessor) && originalName.isSetterName

    /** is this the constructor of a class? */
    def isClassConstructor = name == nme.CONSTRUCTOR

    /** Is this the constructor of a trait? */
    def isTraitConstructor = name == nme.TRAIT_CONSTRUCTOR

    /** Is this the constructor of a trait or a class */
    def isConstructor = name.isConstructorName

    /** Is this a local template dummmy? */
    def isLocalDummy: Boolean = name.isLocalDummyName

    /** Does this symbol denote the primary constructor of its enclosing class? */
    final def isPrimaryConstructor(implicit ctx: Context) =
      isConstructor && owner.primaryConstructor == this

    /** Is this a subclass of `base`,
     *  and is the denoting symbol also different from `Null` or `Nothing`?
     */
    def isNonBottomSubClass(base: Symbol)(implicit ctx: Context) = false

    /** Is this a subclass of `base` or a companion object of such a subclass? */
    final def isSubClassOrCompanion(base: Symbol)(implicit ctx: Context): Boolean =
      isNonBottomSubClass(base) ||
      isModuleClass && linkedClass.isNonBottomSubClass(base)

    /** Is this symbol a class that does not extend `AnyVal`? */
    def isNonValueClass(implicit ctx: Context): Boolean =
      isClass && !isSubClass(defn.AnyValClass)

    /** Is this definition accessible whenever `that` symbol is accessible?
     *  Does not take into account status of protected members.
     */
    def isAsAccessibleAs(that: Symbol)(implicit ctx: Context): Boolean =
      (that.accessBoundary(NoSymbol) isContainedIn this.accessBoundary(NoSymbol)) &&
      (this.isStable || !that.isStable)

    /** Is this definition accessible as a member of tree with type `pre`?
     *  @param pre          The type of the tree from which the selection is made
     *  @param superAccess  Access is via super
     */
    def isAccessibleFrom(pre: Type, superAccess: Boolean = false)(implicit ctx: Context): Boolean = {

      def accessWithinLinked(boundary: Symbol) = {
        val linked = boundary.linkedClass
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /** Are we inside definition of `boundary`? */
      def accessWithin(boundary: Symbol) =
        owner.isContainedIn(boundary) &&
          (!(this is JavaDefined) ||
           owner.enclosingPackage == boundary.enclosingPackage)

      def isCorrectThisType(pre: Type): Boolean = pre match {
        case ThisType(pclazz) =>
          (pclazz eq owner) ||
            (this is Protected) && pclazz.isNonBottomSubClass(owner)
        case _ => false
      }

      /** Is protected access to target symbol permitted? */
      def isProtectedAccessOK = {
        def fail(str: => String): Boolean = {
          ctx.diagnose(str)
          false
        }
        val cls = owner.enclosingSubClass
        if (!cls.exists)
          fail(
            s"""Access to protected $this not permitted because
               |enclosing ${ctx.enclClass.owner.showLocated} is not a subclass of
               |${owner.showLocated} where target is defined""".stripMargin)
        else if (!(isType || // allow accesses to types from arbitrary subclasses fixes #4737
                    pre.widen.typeSymbol.isSubClassOrCompanion(cls) ||
                      cls.isModuleClass &&
                      pre.widen.typeSymbol.isSubClassOrCompanion(cls.linkedClass)))
          fail(
            s"""Access to protected ${symbol.show} not permitted because
               |prefix type ${pre.widen.show} does not conform to
               |${cls.showLocated} where the access takes place""".stripMargin)
        else true
      }

       (pre == NoPrefix) || {
        val boundary = accessBoundary(owner)

        (  (boundary.isTerm
        || (boundary.isRoot))
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

   //    def isOverridable: Boolean = !!! need to enforce that classes cannot be redefined
   //    def isSkolem: Boolean = ???

// ------ access to related symbols ---------------------------------

    /** The class implementing this module, NoSymbol if not applicable. */
    final def moduleClass(implicit ctx: Context): Symbol =
      if (this.isModuleVal) info.typeSymbol else NoSymbol

    /** The module implemented by this module class, NoSymbol if not applicable. */
    final def sourceModule(implicit ctx: Context): Symbol =
      if (this.isModuleClass) this.asClass.selfType.termSymbol else NoSymbol

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

   /** If this is a package object or its implementing class, its owner,
     *  otherwise the denoting symbol.
     */
    final def skipPackageObject(implicit ctx: Context): Symbol =
      if (this is PackageObject) owner else symbol

    /** The owner, skipping package objects. */
    final def effectiveOwner(implicit ctx: Context) = owner.skipPackageObject

    /** The class containing this denotation.
     *  If this denotation is already a class, return itself
     */
    def enclosingClass(implicit ctx: Context): Symbol =
      if (isClass) symbol else owner.enclosingClass

    /** The top-level class containing this denotation,
     *  except for a toplevel module, where its module class is returned.
     */
    final def topLevelClass(implicit ctx: Context): Symbol =
      if (!(owner.isPackageClass)) owner.topLevelClass
      else if (isClass) symbol
      else moduleClass

    /** The package containing this denotation */
    final def enclosingPackage(implicit ctx: Context): Symbol =
      if (isPackageClass) symbol else owner.enclosingPackage

    /** The module object with the same (term-) name as this class or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this module does not exist.
     */
    def companionModule(implicit ctx: Context): Symbol = {
      owner.info.decl(name.toTermName)
        .filter(sym => sym.isModule && sym.isCoDefinedWith(symbol))
        .symbol
    }

    /** The class with the same (type-) name as this module or module class,
     *  and which is also defined in the same scope and compilation unit.
     *  NoSymbol if this class does not exist.
     */
    def companionClass(implicit ctx: Context): Symbol =
      owner.info.decl(name.toTypeName)
        .filter(sym => sym.isClass && sym.isCoDefinedWith(symbol))
        .symbol

    /** If this is a class, the module class of its companion object.
     *  If this is a module class, its companion class.
     *  NoSymbol otherwise.
     */
    def linkedClass(implicit ctx: Context): Symbol =
      if (this.isModuleClass) companionClass
      else if (this.isClass) companionModule.moduleClass
      else NoSymbol

    /** The class that encloses the owner of the current context
     *  and that is a subclass of this class. NoSymbol if no such class exists.
     */
    final def enclosingSubClass(implicit ctx: Context) = {
      val thissym = symbol
      ctx.owner.ownersIterator.findSymbol(_.isSubClass(thissym))
    }

    /** The non-private symbol whose type matches the type of this symbol
     *  in the given class.
     *  @param inClass   The class containing the symbol's definition
     *  @param site      The base type from which member types are computed
     */
     final def matchingSymbol(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = {
      var denot = inClass.info.nonPrivateDecl(name)
      if (denot.isTerm) {
        val targetType = site.memberInfo(symbol)
        if (denot.isOverloaded)
          denot = denot.atSignature(targetType.signature)
        if (!(site.memberInfo(denot.symbol) matches targetType))
          denot = NoDenotation
      }
      denot.symbol
    }

    /** The symbol, in class `inClass`, that is overridden by this denotation. */
    final def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (owner isSubClass inClass) matchingSymbol(inClass, owner.thisType)
      else NoSymbol

    /** All symbols overriden by this denotation. */
    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      info.baseClasses.tail.iterator map overriddenSymbol filter (_.exists)

    /** The class or term symbol up to which this symbol is accessible,
     *  or RootClass if it is public.  As java protected statics are
     *  otherwise completely inaccessible in scala, they are treated
     *  as public.
     *  @param base  The access boundary to assume if this symbol is protected
     */
    final def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = {
      val fs = flags
      if (fs is PrivateOrLocal) owner
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

    /** The type This(cls), where cls is this class, NoPrefix for all other symbols */
    def thisType(implicit ctx: Context): Type = NoPrefix

    /** The named typeref representing the type constructor for this type.
     *  @throws ClassCastException is this is not a type
     */
    def typeConstructor(implicit ctx: Context): TypeRef =
      if (isPackageClass) symbolicRef
      else TypeRef(owner.thisType, name.asTypeName)

    /** The symbolic typeref representing the type constructor for this type.
     *  @throws ClassCastException is this is not a type
     */
    def symbolicRef(implicit ctx: Context): TypeRef =
      TypeRef(owner.thisType, symbol.asType)

    /** The variance of this type parameter as an Int, with
     *  +1 = Covariant, -1 = Contravariant, 0 = Nonvariant, or not a type parameter
     */
    def variance: Int =
      if (this is Covariant) 1
      else if (this is Contravariant) -1
      else 0

 // ----- copies ------------------------------------------------------

    override protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)

    /** Copy this denotation, overriding selective fields */
    def copy(
        sym: Symbol,
        owner: Symbol = this.owner,
        name: Name = this.name,
        initFlags: FlagSet = this.flags,
        privateWithin: Symbol = this.privateWithin,
        info: Type = this.info) =
      CompleteSymDenotation(sym, owner, name, initFlags, info, privateWithin)
  }

  /** The contents of a class definition during a period
   *  Note: important to leave initctx non-implicit, and to check that it is not
   *  retained after object construction.
   */
  abstract class ClassDenotation(initFlags: FlagSet, assocFile: AbstractFile)(initctx: Context)
      extends SymDenotation(initFlags) {
    import NameFilter._
    import util.LRU8Cache

// ----- denotation fields and accessors ------------------------------

    override val symbol: ClassSymbol

    /** The parent types of this class.
     *  These are all normalized to be TypeRefs by moving any refinements
     *  to be member definitions of the class itself. */
    def parents: List[TypeRef]

    /** The type of `this` in this class */
    def selfType: Type

    /** The symbols defined directly in this class */
    def decls: Scope

    def name: TypeName

    override val info = {
      implicit val ctx = initctx
      ClassInfo(owner.thisType, this)
    }

    override def associatedFile(implicit ctx: Context): AbstractFile = assocFile

    private[this] var _typeParams: List[TypeSymbol] = _

    /** The type parameters of this class. Loads the class but does not complete it. */
    override final def typeParams(implicit ctx: Context): List[TypeSymbol] = {
      val tparams = _typeParams
      if (tparams != null) tparams else computeTypeParams
    }

    /** The symbols defined in this class when the class is loaded but
     *  not yet completed.
     */
    protected def preCompleteDecls: Scope

    private def computeTypeParams(implicit ctx: Context): List[TypeSymbol] =
      (preCompleteDecls.toList filter (_ is TypeParam)).asInstanceOf[List[TypeSymbol]]

// ------ class-specific operations -----------------------------------

    private[this] var _thisType: Type = null

    override def thisType(implicit ctx: Context): Type = {
      if (_thisType == null) _thisType = computeThisType
      _thisType
    }

    // todo: apply same scheme to static objects/all objects?
    private def computeThisType(implicit ctx: Context): Type =
      if (isPackageClass && !isRoot)
        TermRef(owner.thisType, name.toTermName)
      else
        ThisType(symbol)

    private[this] var _typeConstructor: TypeRef = null

    override def typeConstructor(implicit ctx: Context): TypeRef = {
      if (_typeConstructor == null) _typeConstructor = super.typeConstructor
      _typeConstructor
    }

    private[this] var _baseClasses: List[ClassSymbol] = null
    private[this] var _superClassBits: BitSet = null

    private def computeSuperClassBits(implicit ctx: Context): Unit = {
      val seen = new mutable.BitSet
      val locked = new mutable.BitSet
      def addBaseClasses(bcs: List[ClassSymbol], to: List[ClassSymbol])
          : List[ClassSymbol] = bcs match {
        case bc :: bcs1 =>
          val id = bc.superId
          if (seen contains id) to
          else if (locked contains id) throw new CyclicReference(symbol)
          else {
            locked += id
            val bcs1added = addBaseClasses(bcs1, to)
            seen += id
            if (bcs1added eq bcs1) bcs else bc :: bcs1added
          }
        case _ =>
          to
      }
      def addParentBaseClasses(ps: List[Type], to: List[ClassSymbol]): List[ClassSymbol] = ps match {
        case p :: ps1 =>
          addBaseClasses(p.baseClasses, addParentBaseClasses(ps1, to))
        case _ =>
          to
      }
      _baseClasses = symbol :: addParentBaseClasses(parents, Nil)
      _superClassBits = ctx.uniqueBits.findEntryOrUpdate(seen.toImmutable)
    }

    private def superClassBits(implicit ctx: Context): BitSet = {
      if (_superClassBits == null) computeSuperClassBits
      _superClassBits
    }

    /** The base classes of this class in linearization order,
     *  with the class itself as first element.
     */
     def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (_baseClasses == null) computeSuperClassBits
      _baseClasses
    }

    final override def isNonBottomSubClass(base: Symbol)(implicit ctx: Context): Boolean =
      (symbol eq base) ||
        (superClassBits contains base.superId) ||
        (this is Erroneous) ||
        (base is Erroneous) && base.isClass

    final override def isSubClass(base: Symbol)(implicit ctx: Context) =
      isNonBottomSubClass(base) ||
        base.isClass && ((symbol eq defn.NothingClass) || (symbol eq defn.NullClass))

    private[this] var _definedFingerPrint: FingerPrint = null

    private def computeDefinedFingerPrint(implicit ctx: Context): FingerPrint = {
      var bits = newNameFilter
      var e = decls.lastEntry
      while (e != null) {
        includeName(bits, name)
        e = e.prev
      }
      var ps = parents
      while (ps.nonEmpty) {
        val parent = ps.head.typeSymbol
        parent.denot match {
          case classd: ClassDenotation =>
            includeFingerPrint(bits, classd.definedFingerPrint)
            parent.denot.setFlag(Frozen)
          case _ =>
        }
        ps = ps.tail
      }
      _definedFingerPrint = bits
      bits
    }

    private[this] var _memberCache: LRU8Cache[Name, DenotationSet] = null

    private def memberCache: LRU8Cache[Name, DenotationSet] = {
      if (_memberCache == null) _memberCache = new LRU8Cache
      _memberCache
    }
    /** Enter a symbol in current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def enter(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls enter sym
      if (_definedFingerPrint != null)
        includeName(_definedFingerPrint, sym.name)
      if (_memberCache != null)
        memberCache invalidate sym.name
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls unlink sym
      if (_definedFingerPrint != null)
        computeDefinedFingerPrint
      if (_memberCache != null)
        memberCache invalidate sym.name
    }

    def definedFingerPrint(implicit ctx: Context): FingerPrint = {
      val fp = _definedFingerPrint
      if (fp != null) fp else computeDefinedFingerPrint
    }

    final def membersNamed(name: Name)(implicit ctx: Context): DenotationSet = {
      var denots: DenotationSet = memberCache lookup name
      if (denots == null) {
        if (containsName(definedFingerPrint, name)) {
          val ownDenots = decls.denotsNamed(name)
          denots = ownDenots
          var ps = parents
          while (ps.nonEmpty) {
            val parentSym = ps.head.typeSymbol
            parentSym.denot match {
              case parentd: ClassDenotation =>
                denots = denots union
                  parentd.membersNamed(name)
                    .filterExcluded(Flags.Private)
                    .asSeenFrom(thisType, parentSym)
                    .filterDisjoint(ownDenots)
              case _ =>
            }
          }
        } else {
          denots = NoDenotation
        }
        memberCache enter (name, denots)
      }
      denots
    }

    private[this] var baseTypeCache: java.util.HashMap[CachedType, Type] = null
    private[this] var baseTypeValid: RunId = NoRunId

    final def baseTypeOf(tp: Type)(implicit ctx: Context): Type = {

      def computeBaseTypeOf(tp: Type): Type = tp match {
        case tp: TypeProxy =>
          baseTypeOf(tp.underlying)
        case AndType(tp1, tp2) =>
          baseTypeOf(tp1) & baseTypeOf(tp2)
        case OrType(tp1, tp2) =>
          baseTypeOf(tp1) | baseTypeOf(tp2)
        case tp @ ClassInfo(pre, classd) =>
          def reduce(bt: Type, ps: List[Type]): Type = ps match {
            case p :: ps1 => reduce(bt & baseTypeOf(p), ps1)
            case _ => bt
          }
          if (classd.symbol == symbol) tp.typeConstructor // was: typeTemplate
          else reduce(NoType, classd.parents).substThis(classd.symbol, tp.prefix)
      }

      if (symbol.isStatic) symbol.typeConstructor
      else tp match {
        case tp: CachedType =>
          if (baseTypeValid != ctx.runId) {
            baseTypeCache = new java.util.HashMap[CachedType, Type]
            baseTypeValid = ctx.runId
          }
          var basetp = baseTypeCache get tp
          if (basetp == null) {
            baseTypeCache.put(tp, NoType)
            basetp = computeBaseTypeOf(tp)
            baseTypeCache.put(tp, basetp)
          } else if (basetp == NoType) {
            throw new CyclicReference(symbol)
          }
          basetp
        case _ =>
          computeBaseTypeOf(tp)
      }
    }

    private[this] var memberNamesCache: Map[NameFilter, Set[Name]] = Map()

    def memberNames(keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      memberNamesCache get keepOnly match {
        case Some(names) =>
          names
        case _ =>
          val inheritedNames = (parents flatMap (_.memberNames(thisType, keepOnly))).toSet
          val ownNames = decls.iterator map (_.name)
          val candidates = inheritedNames ++ ownNames
          val names = candidates filter (keepOnly(thisType, _))
          memberNamesCache = memberNamesCache.updated(keepOnly, names)
          names
    }

    private[this] var fullNameCache: Map[Char, Name] = Map()

    override final def fullName(separator: Char)(implicit ctx: Context): Name =
      fullNameCache get separator match {
      case Some(fn) =>
        fn
      case _ =>
        val fn = super.fullName(separator)
        fullNameCache = fullNameCache.updated(separator, fn)
        fn
    }

    // to avoid overloading ambiguities
    override def fullName(implicit ctx: Context): Name = super.fullName

    override def primaryConstructor(implicit ctx: Context): Symbol = {
      val cname =
        if (this is Trait | ImplClass) nme.TRAIT_CONSTRUCTOR else nme.CONSTRUCTOR
      decls.denotsNamed(cname).first.symbol
    }

    def copyClass(
        sym: ClassSymbol,
        owner: Symbol = this.owner,
        name: TypeName = this.name,
        initFlags: FlagSet = this.flags,
        privateWithin: Symbol = this.privateWithin,
        parents: List[TypeRef] = this.parents,
        selfType: Type = this.selfType,
        decls: Scope = this.decls,
        assocFile: AbstractFile = this.assocFile)(implicit ctx: Context) =
      new CompleteClassDenotation(sym, owner, name, initFlags, parents, privateWithin, selfType, decls, assocFile)(ctx)
  }

// -------- Concrete classes for instantiating denotations --------------------------

  class CompleteSymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val info: Type,
      val privateWithin: Symbol
    ) extends SymDenotation(initFlags)

   def CompleteSymDenotation(symbol: Symbol, owner: Symbol, name: Name, initFlags: FlagSet,
      info: Type, privateWithin: Symbol = NoSymbol) =
    new CompleteSymDenotation(symbol, owner, name, initFlags, info, privateWithin)

  class LazySymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      var completer: SymCompleter
    ) extends SymDenotation(initFlags) with isLazy[LazySymDenotation] {

    final override def exists(implicit ctx: Context) =
      !isModuleVal || moduleClass.denot.exists

    private[this] var _info: Type = _
    protected[core] def info_=(tp: Type) = if (_info == null) _info = tp
    override def info = { if (info == null) tryComplete(); _info }
  }

  def LazySymDenotation(symbol: Symbol, owner: Symbol, name: Name, initFlags: FlagSet,
      completer: SymCompleter) =
    new LazySymDenotation(symbol, owner, name, initFlags, completer)

  class CompleteClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: TypeName,
      initFlags: FlagSet,
      val parents: List[TypeRef],
      val privateWithin: Symbol,
      optSelfType: Type,
      val decls: Scope,
      assocFile: AbstractFile)(initctx: Context)
      extends ClassDenotation(initFlags, assocFile)(initctx) {
    val selfType = if (optSelfType == NoType) typeConstructor(initctx) else optSelfType
    final def preCompleteDecls = decls
  }

  def CompleteClassDenotation(
      symbol: ClassSymbol, owner: Symbol, name: TypeName, initFlags: FlagSet, parents: List[TypeRef],
      privateWithin: Symbol = NoSymbol,
      optSelfType: Type = NoType,
      decls: Scope = newScope,
      assocFile: AbstractFile = null)(implicit ctx: Context) =
    new CompleteClassDenotation(symbol, owner, name, initFlags, parents,
        privateWithin, optSelfType, decls, assocFile)(ctx)

  class LazyClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: TypeName,
      initFlags: FlagSet,
      var completer: ClassCompleter,
      assocFile: AbstractFile
    )(initctx: Context) extends ClassDenotation(initFlags, assocFile)(initctx) with isLazy[LazyClassDenotation] {

    private[this] var _parents: List[TypeRef] = null
    private[this] var _selfType: Type = null
    private[this] var _decls: Scope = null

    protected[core] def parents_=(ps: List[TypeRef]) = if (_parents == null) _parents = ps
    protected[core] def selfType_=(tp: Type) = if (_selfType == null) _selfType = tp
    protected[core] def decls_=(sc: Scope) = if (_decls == null) _decls = sc

    final def parents: List[TypeRef] = { if (_parents == null) tryComplete(); _parents }
    def selfType: Type               = { if (_selfType == null) tryComplete(); _selfType }
    final def preCompleteDecls       = { if (_decls == null) tryLoad(); _decls }
    final def decls: Scope           = { if (_parents == null) tryComplete(); _decls }
      // cannot check on decls because decls might be != null even if class is not completed

    final override def exists(implicit ctx: Context) = { ensureCompleted(); _parents != null }
  }

  def LazyClassDenotation(
      symbol: ClassSymbol, owner: Symbol, name: TypeName, initFlags: FlagSet,
      completer: ClassCompleter, assocFile: AbstractFile = null)(implicit ctx: Context) =
    new LazyClassDenotation(symbol, owner, name, initFlags, completer, assocFile)(ctx)

  object NoDenotation extends SymDenotation(EmptyFlags) {
    override def isTerm = false
    override def isType = false
    override def symbol: Symbol = NoSymbol
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def name: Name = "<none>".toTermName
    override def info: Type = NoType
    override def privateWithin = NoSymbol
    override def exists(implicit ctx: Context) = false
  }

// ---- Completion --------------------------------------------------------

  trait isLazy[Denot <: SymDenotation] extends SymDenotation { this: Denot =>

    protected def completer: Completer[Denot]
    protected def completer_= (c: Completer[Denot])

    override def isLoaded = _privateWithin != null
    override def isCompleted = completer == null

    override protected[core] def tryLoad(): Unit =
      try {
        if (flags is Locked) throw new CyclicReference(symbol)
        setFlag(Locked)
        completer.load(this)
      } finally {
        flags &~= Locked
      }

    override protected[core] def tryComplete(): Unit =
      try {
        if (flags is Locked) throw new CyclicReference(symbol)
        setFlag(Locked)
        val c = completer
        completer = null // set completer to null to avoid space leaks
                         // and to make any subsequent completion attempt a CompletionError
        c.complete(this)
      } catch {
        case _: NullPointerException => throw new CompletionError(this)
      } finally {
        flags &~= Locked
      }

    private[this] var _privateWithin: Symbol = _
    def privateWithin: Symbol = { if (_privateWithin == null) tryLoad(); _privateWithin }
    protected[core] def privateWithin_=(sym: Symbol): Unit = { _privateWithin = sym }
  }

  abstract class Completer[Denot <: SymDenotation] extends DotClass {
    /** Load symbol, setting flags and privateWithin, and typeParams for classes
     *  By default same as complete but can be overridden
     */
    def load(denot: Denot): Unit = complete(denot)

    /** Complete symbol, setting all its properties */
    def complete(denot: Denot): Unit
  }

  type SymCompleter = Completer[LazySymDenotation]
  type ClassCompleter = Completer[LazyClassDenotation]

  class ModuleCompleter(cctx: CondensedContext) extends Completer[LazySymDenotation] {
    implicit protected def ctx: Context = cctx
    def classDenot(denot: LazySymDenotation) =
      denot.moduleClass.denot.asInstanceOf[LazyClassDenotation]
    def copyLoadedFields(denot: LazySymDenotation, from: LazyClassDenotation) = {
      denot.setFlag(from.flags.toTermFlags & RetainedModuleFlags)
      denot.privateWithin = from.privateWithin
    }
    def copyCompletedFields(denot: LazySymDenotation, from: LazyClassDenotation) = {
      copyLoadedFields(denot, from)
      denot.annotations = from.annotations filter (_.appliesToModule)
    }
    override def load(denot: LazySymDenotation): Unit =
      copyLoadedFields(denot, classDenot(denot))
    def complete(denot: LazySymDenotation): Unit =
      copyCompletedFields(denot, classDenot(denot))
  }

  /** A completer for missing references */
  class StubCompleter(cctx: CondensedContext) extends ClassCompleter {
    implicit protected def ctx: Context = cctx

    def initializeToDefaults(denot: LazyClassDenotation) = {
      // todo: initialize to errors instead?
      denot.privateWithin = NoSymbol
      denot.parents = Nil
      denot.selfType = denot.typeConstructor
      denot.decls = EmptyScope
    }

    def complete(denot: LazyClassDenotation): Unit = {
      val sym = denot.symbol
      val file = denot.associatedFile
      val (location, src) =
        if (file != null) (s" in $file", file.toString)
        else ("", "the signature")
      ctx.error(
        s"""|bad symbolic reference. A signature$location refers to ${ctx.showDetailed(denot.name)}
            |in ${denot.owner.showKind} ${denot.owner.showFullName} which is not available.
            |It may be completely missing from the current classpath, or the version on
            |the classpath might be incompatible with the version used when compiling $src.""".stripMargin)
      if (ctx.settings.debug.value) (new Throwable).printStackTrace
      initializeToDefaults(denot)
    }
  }

  class CompletionError(denot: SymDenotation) extends Error("Trying to access missing symbol ${denot.symbol.fullName}")

// ---- Name filter --------------------------------------------------------

  object NameFilter {
    final val WordSizeLog = 6
    final val DefinedNamesWords = 16
    final val DefinedNamesSize = DefinedNamesWords << WordSizeLog
    final val DefinedNamesMask = DefinedNamesSize - 1

    type FingerPrint = Array[Long]

    def includeName(bits: FingerPrint, name: Name): Unit = {
      val hash = name.start & DefinedNamesMask
      bits(hash >> 6) |= (1 << hash)
    }

    def includeFingerPrint(bits1: FingerPrint, bits2: FingerPrint): Unit =
      for (i <- 0 until DefinedNamesWords) bits1(i) |= bits2(i)

    def containsName(bits: FingerPrint, name: Name): Boolean = {
      val hash = name.start & DefinedNamesMask
      (bits(hash >> 6) & (1 << hash)) != 0
    }

    def newNameFilter: FingerPrint = new Array[Long](DefinedNamesWords)
  }

  implicit def toFlagSet(denot: SymDenotation): FlagSet = denot.flags

}
