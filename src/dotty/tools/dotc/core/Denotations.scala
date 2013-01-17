package dotty.tools.dotc
package core

import Periods._, Contexts._, Symbols._, Referenceds._, Names._
import Types._, Flags._, Decorators._, Transformers._
import Scopes.Scope
import collection.mutable
import collection.immutable.BitSet

object Denotations {

  /** A denotation represents the contents of a definition
   *  during a period.
   */
  abstract class Denotation(initFlags: FlagSet) extends SymRefd {

    def symbol: Symbol

    def owner: Symbol

    def name: Name

    private[this] var currentFlags: FlagSet = initFlags

    def flags: FlagSet = currentFlags

    def info: Type

    def setFlag(flag: FlagSet): Unit =
      currentFlags = initFlags

    /** is this symbol a class? */
    def isClass: Boolean = false

    /** is this symbol a method? */
    def isMethod: Boolean = false

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    def withType(tp: Type): Denotation = ???

    override protected def copy(s: Symbol, i: Type): SymRefd = new UniqueSymRefd(s, i, validFor)
  }

  class NonClassDenotation(
      override val symbol: Symbol,
      override val owner: Symbol,
      override val name: Name,
      val initFlags: FlagSet,
      override val info: Type
    ) extends Denotation(initFlags)

  class ClassDenotation(
      override val symbol: ClassSymbol,
      override val owner: Symbol,
      override val name: Name,
      val initFlags: FlagSet,
      val parents: List[Type],
      val decls: Scope)(implicit ctx: Context) extends Denotation(initFlags) {
    import NameFilter._
    import util.LRU8Cache

    def typeParams: List[TypeSymbol] = ???

    val info = ClassInfo(owner.thisType, this)

    private var memberCacheVar: LRU8Cache[Name, ReferencedSet] = null

    private def memberCache: LRU8Cache[Name, ReferencedSet] = {
      if (memberCacheVar == null) memberCacheVar = new LRU8Cache
      memberCacheVar
    }

    private var thisTypeCache: ThisType = null

    def thisType(implicit ctx: Context): Type = {
      if (thisTypeCache == null)
        thisTypeCache = ThisType(symbol)
      thisTypeCache
    }

    private var typeConstructorCache: Type = null

    def typeConstructor(implicit ctx: Context): Type = {
      if (typeConstructorCache == null)
        typeConstructorCache = NamedType(thisType, symbol.name)
      typeConstructorCache
    }

    private var typeTemplateCache: Type = null

    def typeTemplate(implicit ctx: Context): Type = {
      if (typeTemplateCache == null)
        AppliedType.make(typeConstructor, typeParams map (_.typeConstructor))
      typeTemplateCache
    }

    private var baseClassesVar: List[ClassSymbol] = null
    private var superClassBitsVar: BitSet = null

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
      superClassBitsVar = ctx.root.uniqueBits.findEntryOrUpdate(seen.toImmutable)
    }

    def superClassBits(implicit ctx: Context): BitSet = {
      if (superClassBitsVar == null) computeSuperClassBits
      superClassBitsVar
    }

    def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (baseClassesVar == null) computeSuperClassBits
      baseClassesVar
    }

    /** Is this class a subclass of `clazz`? */
    final def isSubClass(clazz: ClassSymbol)(implicit ctx: Context): Boolean = {
      superClassBits contains clazz.superId
    }

    private var definedFingerPrintCache: FingerPrint = null

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
        parent.deref match {
          case classd: ClassDenotation =>
            includeFingerPrint(bits, classd.definedFingerPrint)
            parent.deref setFlag Frozen
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
      require(!flags.hasFlagIn(Frozen))
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
      require((flags & Frozen) == Flags.Empty)
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

    final def memberRefsNamed(name: Name)(implicit ctx: Context): ReferencedSet = {
      var refs: ReferencedSet = memberCache lookup name
      if (refs == null) {
        if (containsName(definedFingerPrint, name)) {
          val ownRefs = decls.refsNamed(name)
          refs = ownRefs
          var ps = parents
          while (ps.nonEmpty) {
            val parentSym = ps.head.typeSymbol
            parentSym.deref match {
              case parentd: ClassDenotation =>
                refs = refs union
                  parentd.memberRefsNamed(name)
                    .filterExcluded(Flags.Private)
                    .asSeenFrom(thisType, parentSym)
                    .filterDisjoint(ownRefs)
              case _ =>
            }
          }
        } else {
          refs = NoRefd
        }
        memberCache enter (name, refs)
      }
      refs
    }

    private var baseTypeCache: java.util.HashMap[CachedType, Type] = null
    private var baseTypeValid: RunId = NoRunId

    final def baseTypeOf(tp: Type)(implicit ctx: Context): Type = {

      def computeBaseTypeOf(tp: Type): Type = tp match {
        case AppliedType(tycon, args) =>
          baseTypeOf(tycon).subst(tycon.typeParams, args)
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
          if (classd.symbol == symbol) tp.typeTemplate
          else reduce(NoType, classd.parents).substThis(classd.symbol, tp.prefix)
      }

      if (symbol.isStaticMono) symbol.typeConstructor
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

    private var memberNamesCache: Map[NameFilter, Set[Name]] = Map()

    def memberNames(keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      memberNamesCache get keepOnly match {
        case Some(names) =>
          names
        case _ =>
          val inheritedNames = (parents flatMap (_.memberNames(thisType, keepOnly))).toSet
          val ownNames = decls.iterator map (_.name)
          val candidates = inheritedNames ++ ownNames
          val names = candidates filter (keepOnly(thisType, _))
          memberNamesCache += (keepOnly -> names)
          names
    }
  }

  object NoDenotation extends Denotation(Flags.Empty) {
    override def symbol: Symbol = NoSymbol
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def name: Name = BootNameTable.newTermName("<none>")
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
}