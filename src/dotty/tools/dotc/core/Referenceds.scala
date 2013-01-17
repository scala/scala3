package dotty.tools.dotc
package core

import Denotations.Denotation
import Contexts.Context
import Names.Name
import Names.TypeName
import Symbols.NoSymbol
import Symbols.Symbol
import Types._, Periods._, Flags._, Transformers._


/** Classes that implement referenced items and sets of them
 */
object Referenceds {

  /** The signature of a referenced.
   *  Overloaded referenceds with the same name are distinguished by
   *  their signatures. A signature is a list of the fully qualified names
   *  of the type symbols of the erasure of the parameters of the
   *  referenced. For instance a referenced definition
   *
   *      def f(x: Int)(y: List[String]): String
   *
   *  would have signature
   *
   *      List("scala.Int".toTypeName, "scala.collection.immutable.List".toTypeName)
   */
  type Signature = List[TypeName]

  /** The signature of a val or parameterless def, as opposed
   *  to List(), which is the signature of a zero-parameter def.
   */
  val NullSignature = List(Names.EmptyTypeName)

  /** A referenced is the result of resolving
   *  a name (either simple identifier or select) during a given period.
   *
   *  Referenced has two subclasses: OverloadedRefd and SymRefd.
   *
   *  A SymRefd refers to a `symbol` and a type (`info`) that the symbol has
   *  when seen from the reference.
   *
   *  Referenceds can be combined with `&` and `|`.
   *  & is conjunction, | is disjunction.
   *
   *  `&` will create an overloaded reference from two
   *  non-overloaded references if their signatures differ.
   *  Analogously `|` of two references with different signatures will give
   *  an empty reference `NoRefd`.
   *
   *  A referenced might refer to `NoSymbol`. This is the case if the referenced
   *  was produced from a disjunction of two referenceds with different symbols
   *  and there was no common symbol in a superclass that could substitute for
   *  both symbols. Here is an example:
   *
   *  Say, we have:
   *
   *    class A { def f: A }
   *    class B { def f: B }
   *    val x: A | B = if (???) new A else new B
   *    val y = x.f
   *
   *  Then the referenced of `y` is `SymRefd(NoSymbol, A | B)`.
   */
  abstract class Referenced extends DotClass {

    /** The referenced symbol, exists only for non-overloaded references */
    def symbol: Symbol

    /** The type info of the reference, exists only for non-overloaded references */
    def info: Type

    /** The interval during which this reference is valid */
    def validFor: Period

    /** Is this a reference to a type symbol? */
    def isType: Boolean = false

    /** The signature of the reference */
    def signature: Signature

    /** Resolve overloaded reference to pick the one with the given signature */
    def atSignature(sig: Signature): Referenced

    /** The variant of this reference that's current in the given context. */
    def current(implicit ctx: Context): Referenced

    def exists: Boolean = true

    def orElse(that: => Referenced) = if (this.exists) this else that

    /** Form a reference by conjoining with reference `that` */
    def & (that: Referenced)(implicit ctx: Context): Referenced =
      if (this eq that) this
      else if (!this.exists) that
      else if (!that.exists) this
      else that match {
        case that: SymRefd =>
          val r = mergeRef(this, that)
          if (r ne NoRefd) r else OverloadedRef(this, that)
        case that @ OverloadedRef(ref1, ref2) =>
          this & ref1 & ref2
      }

    /** Try to merge ref1 and ref2 without adding a new signature.
     *  If unsuccessful, return NoRefd.
     */
    private def mergeRef(ref1: Referenced, ref2: SymRefd)(implicit ctx: Context): Referenced = ref1 match {
      case ref1 @ OverloadedRef(ref11, ref12) =>
        val r1 = mergeRef(ref11, ref2)
        if (r1 ne NoRefd) r1 else mergeRef(ref12, ref2)
      case ref1: SymRefd =>
        if (ref1 eq ref2) ref1
        else if (ref1.signature == ref2.signature) {
          def isEligible(sym1: Symbol, sym2: Symbol) =
            if (sym1.isType) !sym1.isClass
            else sym1.isConcrete || sym2.isDeferred || !sym2.exists
          def normalize(info: Type) =
            if (isType) info.bounds else info
          val sym1 = ref1.symbol
          val info1 = ref1.info
          val sym2 = ref2.symbol
          val info2 = ref2.info
          val sym1Eligible = isEligible(sym1, sym2)
          val sym2Eligible = isEligible(sym2, sym1)
          val bounds1 = normalize(info1)
          val bounds2 = normalize(info2)
          if (sym2Eligible && bounds2 <:< bounds1) ref2
          else if (sym1Eligible && bounds1 <:< bounds2) ref1
          else new JointSymRefd(
              if (sym2Eligible) sym2 else sym1,
              bounds1 & bounds2,
              ref1.validFor & ref2.validFor)
        } else NoRefd
    }

    def | (that: Referenced)(pre: Type)(implicit ctx: Context): Referenced = {

      def lubSym(sym1: Symbol, sym2: Symbol): Symbol = {
        def qualifies(sym: Symbol) =
          (sym isAccessibleFrom pre) && (sym2.owner isSubClass sym.owner)
        sym1.allOverriddenSymbols find qualifies getOrElse NoSymbol
      }

      def throwError = throw new MatchError(s"orRef($this, $that)")

      if (this eq that) this
      else if (!this.exists) this
      else if (!that.exists) that
      else this match {
        case ref1 @ OverloadedRef(ref11, ref12) =>
          ref1.derivedOverloadedRef((ref11 | that)(pre), (ref12 | that)(pre))
        case _ =>
          that match {
            case ref2 @ OverloadedRef(ref21, ref22) =>
              ref2.derivedOverloadedRef((this | ref21)(pre), (this | ref22)(pre))
            case ref2: SymRefd =>
              this match {
                case ref1: SymRefd =>
                    if (ref1.signature != ref2.signature) NoRefd
                    else new JointSymRefd(
                      lubSym(ref1.symbol, ref2.symbol),
                      ref1.info | ref2.info,
                      ref1.validFor & ref2.validFor)
                  case _ =>
                    throwError
                }
              case _ =>
                throwError
            }
        }
    }
  }

  /** The class of overloaded references
   *  @param  variants   The overloaded variants indexed by thheir signatures.
   */
  case class OverloadedRef(ref1: Referenced, ref2: Referenced) extends Referenced {
    def derivedOverloadedRef(r1: Referenced, r2: Referenced) =
      if ((r1 eq ref1) && (r2 eq ref2)) this else OverloadedRef(r1, r2)
    def symbol = unsupported("symbol")
    def info = unsupported("info")
    def signature = unsupported("signature")
    def atSignature(sig: Signature): Referenced =
      ref1.atSignature(sig) orElse ref2.atSignature(sig)
    def validFor = ref1.validFor & ref2.validFor
    def current(implicit ctx: Context): Referenced =
      derivedOverloadedRef(ref1.current, ref2.current)
  }

  abstract class SymRefd extends Referenced with ReferencedSet {

    override def isType = symbol.isType
    override def signature: Signature = {
      def sig(tp: Type): Signature = tp match {
        case tp: PolyType =>
          tp.resultType match {
            case mt: MethodType => mt.signature
            case _ => List()
          }
        case mt: MethodType => mt.signature
        case _ => NullSignature
      }
      if (isType) NullSignature else sig(info)
    }

    def derivedSymRefd(s: Symbol, i: Type): SymRefd =
      if ((s eq symbol) && (i eq info)) this else copy(s, i)

    protected def copy(s: Symbol, i: Type): SymRefd = this

    def atSignature(sig: Signature): Referenced =
      if (sig == signature) this else NoRefd

    // ------ Transformations -----------------------------------------

    var validFor: Period = Nowhere

    /** The next SymRefd in this run, with wrap-around from last to first. */
    var nextInRun: SymRefd = this

    /** The version of this SymRefd that was valid in the first phase
     *  of this run.
     */
    def initial: SymRefd = {
      var current = nextInRun
      while (current.validFor.code > this.validFor.code) current = current.nextInRun
      current
    }

    def current(implicit ctx: Context): SymRefd = {
      val currentPeriod = ctx.period
      val valid = validFor
      var current = this
      if (currentPeriod.code > valid.code) {
        // search for containing period as long as nextInRun increases.
        var next = nextInRun
        while (next.validFor.code > valid.code &&
               !(next.validFor contains currentPeriod)) {
          current = next
          next = next.nextInRun
        }
        if (next.validFor.code > valid.code) {
          // in this case, containsPeriod(next.validFor, currentPeriod)
          current = next
        } else {
          // not found, current points to highest existing variant
          var startPid = current.validFor.lastPhaseId + 1
          val trans = ctx.root.transformersFor(current)
          val endPid = trans.nextTransformer(startPid + 1).phaseId - 1
          next = trans.nextTransformer(startPid) transform current
          if (next eq current)
            startPid = current.validFor.firstPhaseId
          else {
            current.nextInRun = next
            current = next
          }
          current.validFor = Period(currentPeriod.runId, startPid, endPid)
        }
      } else {
        // currentPeriod < valid; in this case a version must exist
        do {
          current = current.nextInRun
        } while (!(current.validFor contains currentPeriod))
      }
      current
    }

    def asDenotation = asInstanceOf[Denotation]

    // ------ ReferencedSet ops ----------------------------------------------

    def toRef(implicit ctx: Context) = this
    def containsSig(sig: Signature)(implicit ctx: Context) =
      signature == sig
    def filter(p: Symbol => Boolean)(implicit ctx: Context): ReferencedSet =
      if (p(symbol)) this else NoRefd
    def filterDisjoint(refs: ReferencedSet)(implicit ctx: Context): ReferencedSet =
      if (refs.containsSig(signature)) NoRefd else this
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): ReferencedSet =
      if (symbol.hasFlag(flags)) NoRefd else this
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): ReferencedSet =
      if (symbol.isAccessibleFrom(pre)) this else NoRefd
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): ReferencedSet =
      derivedSymRefd(symbol, info.asSeenFrom(pre, owner))
  }

  class UniqueSymRefd(val symbol: Symbol,
                     val info: Type,
                     initValidFor: Period) extends SymRefd {
    validFor = initValidFor
    override protected def copy(s: Symbol, i: Type): SymRefd = new UniqueSymRefd(s, i, validFor)
  }

  class JointSymRefd(val symbol: Symbol,
                    val info: Type,
                    initValidFor: Period) extends SymRefd {
    validFor = initValidFor
    override protected def copy(s: Symbol, i: Type): SymRefd = new JointSymRefd(s, i, validFor)
  }

  class ErrorRefd(implicit ctx: Context) extends SymRefd {
    val symbol = NoSymbol
    val info = NoType
    validFor = Period.allInRun(ctx.runId)
  }

  object NoRefd extends SymRefd {
    val symbol = NoSymbol
    val info = NoType
    validFor = Nowhere
    override def exists = false
  }

// --------------- ReferencedSets -------------------------------------------------

  /** A ReferencedSet represents a set of referenced */
  trait ReferencedSet {
    def exists: Boolean
    def toRef(implicit ctx: Context): Referenced
    def containsSig(sig: Signature)(implicit ctx: Context): Boolean
    def filter(p: Symbol => Boolean)(implicit ctx: Context): ReferencedSet
    def filterDisjoint(refs: ReferencedSet)(implicit ctx: Context): ReferencedSet
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): ReferencedSet
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): ReferencedSet
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): ReferencedSet
    def union(that: ReferencedSet) =
      if (!this.exists) that
      else if (that.exists) this
      else RefUnion(this, that)
  }

  case class RefUnion(refs1: ReferencedSet, refs2: ReferencedSet) extends ReferencedSet {
    assert(refs1.exists && !refs2.exists)
    private def derivedUnion(s1: ReferencedSet, s2: ReferencedSet) =
      if (!s1.exists) s2
      else if (!s2.exists) s1
      else if ((s1 eq refs2) && (s2 eq refs2)) this
      else new RefUnion(s1, s2)
    def exists = true
    def toRef(implicit ctx: Context) = refs1.toRef & refs2.toRef
    def containsSig(sig: Signature)(implicit ctx: Context) =
      (refs1 containsSig sig) || (refs2 containsSig sig)
    def filter(p: Symbol => Boolean)(implicit ctx: Context) =
      derivedUnion(refs1 filter p, refs2 filter p)
    def filterDisjoint(refs: ReferencedSet)(implicit ctx: Context): ReferencedSet =
      derivedUnion(refs1 filterDisjoint refs, refs2 filterDisjoint refs)
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): ReferencedSet =
      derivedUnion(refs1 filterExcluded flags, refs2 filterExcluded flags)
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): ReferencedSet =
      derivedUnion(refs1 filterAccessibleFrom pre, refs2 filterAccessibleFrom pre)
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): ReferencedSet =
      derivedUnion(refs1.asSeenFrom(pre, owner), refs2.asSeenFrom(pre, owner))
  }
}

