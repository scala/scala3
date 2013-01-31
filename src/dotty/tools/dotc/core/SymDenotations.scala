package dotty.tools.dotc
package core

import Periods._, Contexts._, Symbols._, Denotations._, Names._, Annotations._
import Types._, Flags._, Decorators._, Transformers._, StdNames._
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

    def owner: Symbol

    def name: Name

    def symbol: Symbol

    def info: Type

    final def isLoaded = _privateWithin != null
    final def isCompleted = _annotations != null
    final def ensureLoaded() = if (!isLoaded) tryLoad()
    final def ensureCompleted() = if (!isCompleted) tryComplete()
    protected def tryLoad(): Unit
    protected def tryComplete(): Unit

    private[this] var _flags: FlagSet = initFlags

    def flags: FlagSet = { ensureLoaded(); _flags }
    def flags_=(flags: FlagSet): Unit = { _flags |= flags }
    def setFlags(flags: FlagSet): Unit = { _flags |= flags }
    def resetFlags(flags: FlagSet): Unit = { _flags &~= flags }

    private[this] var _privateWithin: Symbol = _
    def privateWithin: Symbol = { ensureLoaded(); _privateWithin }
    def privateWithin_=(sym: Symbol): Unit = { _privateWithin = sym }

    private[this] var _annotations: List[Annotation] = { ensureCompleted(); _annotations }
    def annotations: List[Annotation] = _annotations
    def annotations_=(annots: List[Annotation]): Unit = { _annotations = annots }

    def hasAnnotation(cls: Symbol) = dropOtherAnnotations(annotations, cls).nonEmpty

    @tailrec
    private def dropOtherAnnotations(anns: List[Annotation], cls: Symbol): List[Annotation] = anns match {
      case ann :: rest => if (ann matches cls) anns else dropOtherAnnotations(rest, cls)
      case Nil => Nil
    }

    /** is this denotation a class? */
    final def isClass: Boolean = symbol.isInstanceOf[ClassSymbol]

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

    def isSubClass(cls: Symbol)(implicit ctx: Context) = false

    def isNonBottomSubClass(cls: Symbol)(implicit ctx: Context) = false

    final def isSubClassOrCompanion(base: Symbol)(implicit ctx: Context): Boolean =
      isNonBottomSubClass(base) ||
      isModuleClass && linkedClass.isNonBottomSubClass(base)

    final def enclosingSubClass(implicit ctx: Context) = {
      val thissym = symbol
      ctx.owner.ownersIterator.findSymbol(_.isSubClass(thissym))
    }

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    final def ownersIterator(implicit ctx: Context) = new Iterator[Symbol] {
      private[this] var current = symbol
      def hasNext = current.exists
      def next: Symbol = {
        val result = current
        current = current.owner
        result
      }
    }

    final def hasTransOwner(sym: Symbol)(implicit ctx: Context): Boolean = {
      var o = symbol
      while ((o ne sym) && (o ne NoSymbol)) o = o.owner
      (o eq sym)
    }

//    def withType(tp: Type): SymDenotation = ???

    override protected def copy(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)

    def moduleClass(implicit ctx: Context): Symbol =
      if (this.isModuleVal) info.typeSymbol else NoSymbol

    /** Desire to re-use the field in ClassSymbol which stores the source
     *  file to also store the classfile, but without changing the behavior
     *  of sourceFile (which is expected at least in the IDE only to
     *  return actual source code.) So sourceFile has classfiles filtered out.
     */
    private def sourceFileOnly(file: AbstractFile): AbstractFile =
      if ((file eq null) || (file.path endsWith ".class")) null else file

    private def binaryFileOnly(file: AbstractFile): AbstractFile =
      if ((file eq null) || !(file.path endsWith ".class")) null else file

    final def effectiveOwner(implicit ctx: Context) = owner.skipPackageObject

    def enclosingClass(implicit ctx: Context): Symbol =
      if (isClass) symbol else owner.enclosingClass

    final def topLevelClass(implicit ctx: Context): Symbol =
      if (!(owner.isPackageClass)) owner.topLevelClass
      else if (isClass) symbol
      else moduleClass

    final def enclosingPackage(implicit ctx: Context): Symbol =
      if (isPackageClass) symbol else owner.enclosingPackage

    def fullName(separator: Char)(implicit ctx: Context): Name =
      if (this == NoSymbol || owner == NoSymbol || owner.isEffectiveRoot) name
      else (effectiveOwner.enclosingClass.fullName(separator) :+ separator) ++ name

    def associatedFile(implicit ctx: Context): AbstractFile = topLevelClass.associatedFile
    final def binaryFile(implicit ctx: Context): AbstractFile = binaryFileOnly(associatedFile)
    final def sourceFile(implicit ctx: Context): AbstractFile = sourceFileOnly(associatedFile)

    /** Is this symbol a type or stable term? */
    final def isStable(implicit ctx: Context) = !(
      isTerm &&
      this.is(UnstableValue, butNot = Stable) ||
      info.isVolatile && !hasAnnotation(defn.uncheckedStableClass)
    )

    def isStatic(implicit ctx: Context) = (this is Static) || owner.isStaticOwner

    final def isStaticOwner(implicit ctx: Context): Boolean =
      isPackageClass || isModuleClass && isStatic

    final def matchingSymbol(inClass: Symbol, site: Type)(implicit ctx: Context): Symbol = {
      var denot = inClass.info.nonPrivateDecl(name)
      if (denot.isTerm) {
        val targetType = site.memberInfo(this)
        if (denot.isOverloaded)
          denot = denot.atSignature(targetType.signature)
        if (!(site.memberInfo(denot.asInstanceOf[SymDenotation]) matches targetType))
          denot = NoDenotation
      }
      denot.symbol
    }

    final def overriddenSymbol(inClass: ClassSymbol)(implicit ctx: Context): Symbol =
      if (owner isSubClass inClass) matchingSymbol(inClass, owner.thisType)
      else NoSymbol

    final def allOverriddenSymbols(implicit ctx: Context): Iterator[Symbol] =
      info.baseClasses.tail.iterator map overriddenSymbol filter (_.exists)

    def isCoDefinedWith(that: Symbol)(implicit ctx: Context) =
      (this.effectiveOwner == that.effectiveOwner) &&
      (  !(this.effectiveOwner.isPackageClass)
      || (this.sourceFile == null)
      || (that.sourceFile == null)
      || (this.sourceFile.path == that.sourceFile.path)  // Cheap possibly wrong check, then expensive normalization
      || (this.sourceFile.canonicalPath == that.sourceFile.canonicalPath)
      )

    def companionModule(implicit ctx: Context): Symbol = {
      owner.info.decl(name.toTermName)
        .filter(sym => sym.isModule && sym.isCoDefinedWith(symbol))
        .symbol
    }

    def companionClass(implicit ctx: Context): Symbol =
      owner.info.decl(name.toTypeName)
        .filter(sym => sym.isClass && sym.isCoDefinedWith(symbol))
        .symbol

    def linkedClass(implicit ctx: Context): Symbol =
      if (this.isModuleClass) companionClass
      else if (this.isClass) companionModule.moduleClass
      else NoSymbol

    final def accessBoundary(base: Symbol)(implicit ctx: Context): Symbol = {
      val fs = flags
      if (fs is PrivateOrLocal) owner
      else if (fs is StaticProtected) defn.RootClass
      else if (privateWithin.exists && !ctx.phase.erasedTypes) privateWithin
      else if (fs is Protected) base
      else defn.RootClass
    }

    final def isContainedIn(boundary: Symbol)(implicit ctx: Context): Boolean =
      if (symbol eq boundary) true
      else if (!this.exists ||
               (this.isPackageClass) && !(boundary.isPackageClass)) false
      else owner.isContainedIn(boundary)

    def isAsAccessibleAs(that: Symbol)(implicit ctx: Context): Boolean =
     (that.accessBoundary(NoSymbol) isContainedIn this.accessBoundary(NoSymbol)) &&
     (this.isStable || !that.isStable)

    def isAccessibleFrom(pre: Type, superAccess: Boolean = false)(implicit ctx: Context): Boolean = {

      def accessWithinLinked(boundary: Symbol) = {
        val linked = boundary.linkedClass
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /** Are we inside definition of `boundary`? */
      def accessWithin(boundary: Symbol) =
        owner.hasTransOwner(boundary) &&
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

    def isNonValueClass(implicit ctx: Context): Boolean =
      isClass && !isSubClass(defn.AnyValClass)

    def typeParams: List[TypeSymbol] = unsupported("typeParams")

    def thisType(implicit ctx: Context): Type = unsupported("thisType")

    def typeConstructor(implicit ctx: Context): TypeRef =
      TypeRef(owner.thisType, name.asTypeName)

    def variance: Int =
      if (this is Covariant) 1
      else if (this is Contravariant) -1
      else 0

    def isRoot: Boolean = name.toTermName == nme.ROOT

    def isEmptyPackage(implicit ctx: Context): Boolean = name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    def isEffectiveRoot(implicit ctx: Context) = isRoot || isEmptyPackage

    def isAnonymousClass(implicit ctx: Context): Boolean = initial.asSymDenotation.name startsWith tpnme.ANON_CLASS

    def copy(
        sym: Symbol,
        owner: Symbol = this.owner,
        name: Name = this.name,
        initFlags: FlagSet = this.flags,
        info: Type = this.info) =
      new CompleteSymDenotation(sym, owner, name, initFlags, info)
  }

  trait isComplete extends SymDenotation {
    privateWithin = NoSymbol
    annotations = Nil
    def tryLoad(): Unit = unsupported("tryLoad")
    def tryComplete(): Unit = unsupported("tryComplete")
  }

  abstract class Completer[Denot <: SymDenotation] {
    def handleCycle(denot: Denot): Unit
    def load(denot: Denot): Unit
    def complete(denot: Denot): Unit
  }

  type SymCompleter = Completer[LazySymDenotation]
  type ClassCompleter = Completer[LazyClassDenotation]

  class ModuleCompleter(moduleClass: ClassSymbol) extends Completer[LazySymDenotation] {
    def handleCycle(denot: LazySymDenotation): Unit = ???
    def load(denot: LazySymDenotation): Unit = ???
    def complete(denot: LazySymDenotation): Unit = ???
  }

  trait isLazy[Denot <: SymDenotation] extends SymDenotation { this: Denot =>

    protected def completer: Completer[Denot]

    protected def tryLoad(): Unit = try {
      if (flags is Locked) throw new CyclicReference(symbol)
      setFlags(Locked)
      completer.load(this)
    } catch {
      case ex: CyclicReference => completer.handleCycle(this)
    } finally {
      flags &~= Locked
    }

    protected def tryComplete(): Unit = try {
      if (flags is Locked) throw new CyclicReference(symbol)
      completer.complete(this)
    } catch {
      case ex: CyclicReference => completer.handleCycle(this)
    } finally {
      flags &~= Locked
    }
  }

  class CompleteSymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val info: Type
    ) extends SymDenotation(initFlags) with isComplete

  class LazyModuleDenotation(
      symbol: Symbol,
      owner: Symbol,
      name: Name,
      initFlags: FlagSet,
      val moduleClass: ClassSymbol
    ) extends LazySymDenotation(symbol, owner, name, initFlags, new ModuleCompleter(moduleClass)) {

    override val info: Type = ???
  }


  class LazySymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val completer: SymCompleter
    ) extends SymDenotation(initFlags) with isLazy[LazySymDenotation] {

    private[this] var _info: Type = _

    override def info = { ensureCompleted(); _info }
  }

  abstract class ClassDenotation(initFlags: FlagSet, assocFile: AbstractFile)(implicit ctx: Context)
      extends SymDenotation(initFlags) {
    import NameFilter._
    import util.LRU8Cache

    val symbol: ClassSymbol

    def parents: List[TypeRef]

    def selfType: Type

    def decls: Scope

    val info = ClassInfo(owner.thisType, this)

    override def associatedFile(implicit ctx: Context): AbstractFile = assocFile

    private[this] var _typeParams: List[TypeSymbol] = _

    override final def typeParams: List[TypeSymbol] = {
      val tparams = _typeParams
      if (tparams != null) tparams else computeTypeParams
    }

    private def computeTypeParams: List[TypeSymbol] =
      (preCompleteDecls.toList filter (_ is TypeParam)).asInstanceOf[List[TypeSymbol]]

    protected def preCompleteDecls: Scope

    private[this] var memberCacheVar: LRU8Cache[Name, DenotationSet] = null

    private def memberCache: LRU8Cache[Name, DenotationSet] = {
      if (memberCacheVar == null) memberCacheVar = new LRU8Cache
      memberCacheVar
    }

    private[this] var _thisType: Type = null

    override def thisType(implicit ctx: Context): Type = {
      if (_thisType == null) _thisType = computeThisType
      _thisType
    }

    // todo: apply same scheme to static objects/all objects?
    private def computeThisType: Type =
      if (isPackageClass && !isRoot)
        TermRef(owner.thisType, name.toTermName)
      else
        ThisType(symbol)

    private[this] var _typeConstructor: TypeRef = null

    override def typeConstructor(implicit ctx: Context): TypeRef = {
      if (_typeConstructor == null) _typeConstructor = super.typeConstructor
      _typeConstructor
    }

 /*
    private[this] var typeTemplateCache: Type = null

    def typeTemplate(implicit ctx: Context): Type = {
      if (typeTemplateCache == null)
        AppliedType.make(typeConstructor, typeParams map (_.typeConstructor))
      typeTemplateCache
    }
*/
    private[this] var baseClassesVar: List[ClassSymbol] = null
    private[this] var superClassBitsVar: BitSet = null

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
      baseClassesVar = symbol :: addParentBaseClasses(parents, Nil)
      superClassBitsVar = ctx.uniqueBits.findEntryOrUpdate(seen.toImmutable)
    }

    def superClassBits(implicit ctx: Context): BitSet = {
      if (superClassBitsVar == null) computeSuperClassBits
      superClassBitsVar
    }

    def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (baseClassesVar == null) computeSuperClassBits
      baseClassesVar
    }

    final override def isNonBottomSubClass(cls: Symbol)(implicit ctx: Context): Boolean =
      (symbol eq cls) ||
        (superClassBits contains cls.superId) ||
        (this is Erroneous) ||
        (cls is Erroneous) && cls.isClass

    final override def isSubClass(cls: Symbol)(implicit ctx: Context) =
      isNonBottomSubClass(cls) ||
        cls.isClass && ((symbol eq defn.NothingClass) || (symbol eq defn.NullClass))

    private[this] var definedFingerPrintCache: FingerPrint = null

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
            parent.denot.setFlags(Frozen)
          case _ =>
        }
        ps = ps.tail
      }
      definedFingerPrintCache = bits
      bits
    }

    /** Enter a symbol in current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def enter(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls enter sym
      if (definedFingerPrintCache != null)
        includeName(definedFingerPrintCache, sym.name)
      if (memberCacheVar != null)
        memberCache invalidate sym.name
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls unlink sym
      if (definedFingerPrintCache != null)
        computeDefinedFingerPrint
      if (memberCacheVar != null)
        memberCache invalidate sym.name
    }

    def definedFingerPrint(implicit ctx: Context): FingerPrint = {
      val fp = definedFingerPrintCache
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

    def copyClass(
        sym: ClassSymbol,
        owner: Symbol = this.owner,
        name: Name = this.name,
        initFlags: FlagSet = this.flags,
        parents: List[TypeRef] = this.parents,
        selfType: Type = this.selfType,
        decls: Scope = this.decls,
        assocFile: AbstractFile = this.assocFile) =
      new CompleteClassDenotation(sym, owner, name, initFlags, parents, selfType, decls, assocFile)
  }

  class CompleteClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val parents: List[TypeRef],
      optSelfType: Type,
      val decls: Scope,
      assocFile: AbstractFile = null
  )(implicit ctx: Context) extends ClassDenotation(initFlags, assocFile) with isComplete {
    val selfType = if (optSelfType == NoType) thisType else optSelfType
    final def preCompleteDecls = decls
  }

  class LazyClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val completer: ClassCompleter,
      assocFile: AbstractFile = null
    )(implicit ctx: Context) extends ClassDenotation(initFlags, assocFile) with isLazy[LazyClassDenotation] {

    protected var _parents: List[TypeRef] = null
    protected var _selfType: Type = null
    protected var _decls: Scope = null

    final def parents: List[TypeRef] = { ensureCompleted(); _parents }
    final def selfType: Type = { ensureCompleted(); _selfType }
    final def decls: Scope = { ensureCompleted(); _decls }
    final def preCompleteDecls = {ensureLoaded(); _decls }
  }

  object NoDenotation extends SymDenotation(EmptyFlags) with isComplete {
    override def symbol: Symbol = NoSymbol
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def name: Name = "<none>".toTermName
    override def info: Type = NoType
  }

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
