package dotty.tools
package dotc
package core

import SymDenotations.{ SymDenotation, NoDenotation }
import Contexts.{Context, ContextBase}
import Names.Name
import Names.TypeName
import Symbols.NoSymbol
import Symbols._
import Types._, Periods._, Flags._, Transformers._
import io.AbstractFile
import Decorators.SymbolIteratorDecorator



/** Denotations represent the meaning of symbols and named types.
 *  The following diagram shows how the principal types of denotations
 *  and their denoting entities relate to each other. Lines ending in
 *  a down-arrow `v` are member methods. The two methods shown in the diagram are
 *  "symbol" and "deref". Both methods are parameterized by the current context,
 *  and are effectively indexed by current period.
 *
 *  Lines ending in a horizontal line mean subtying (right is a subtype of left).
 *
 *  NamedType------NamedTypeWithSignature
 *
 *    |                    |                     Symbol---------ClassSymbol
 *    |                    |                       |                |
 *    | denot              | denot                 | denot          | denot
 *    v                    v                       v                v
 *  Denotation-+-----SingleDenotation-+------SymDenotation-+----ClassDenotation-+--CompleteClassDenotation
 *             |                      |                    |                    +--LazyClassDenotation
 *             +-----MultiDenotation  |                    |
 *                                    |                    +--CompleteSymDenotation
 *                                    |                    +--LazySymDenotation
 *                                    |
 *                                    +--UniqueRefDenotation
 *                                    +--JointRefDenotation
 *
 *  Here's a short summary of the classes in this diagram.
 *
 *  NamedType                A type consisting of a prefix type and a name, with fields
 *                              prefix: Type
 *                              name: Name
 *  NamedTypeWithSignature   A named type that has in addition a signature to select an overloaded variant, with new field
 *                              signature: Signature
 *  Symbol                   A label for a definition or declaration in one compiler run
 *  ClassSymbol              A symbol representing a class
 *  Denotation               The meaning of a named type or symbol during a period
 *  MultiDenotation          A denotation representing several overloaded members
 *  SingleDenotation         A denotation representing a non-overloaded member or definition, with main fields
 *                              symbol: Symbol
 *                              info: Type
 *  UniqueRefDenotation      A denotation referring to a single definition with some member type
 *  JointRefDenotation       A denotation referring to a member that could resolve to several definitions
 *  SymDenotation            A denotation representing a single definition with its original type, with main fields
 *                              name: Name
 *                              owner: Symbol
 *                              flags: Flags
 *                              privateWithin: Symbol
 *                              annotations: List[Annotation]
 *  ClassDenotation          A denotation representing a single class definition, with new fields
 *                              typeParams: List[TypeSymbol]
 *                              parents: List[Type]
 *                              decls: Scope
 *  LazySymDenotation        A sym-denotation with fields that are computed on demand
 *  CompleteSymDenotation    A sym-denotation that has all fields completed
 *  LazyClassDenotation      A class denotation with fields that are computed on demand
 *  CompleteClassDenotation  A class denotation that has all fields completed
 */
object Denotations {

  /** The signature of a denotation.
   *  Overloaded denotations with the same name are distinguished by
   *  their signatures. A signature is a list of the fully qualified names
   *  of the type symbols of the erasure of the parameters of the
   *  denotation. For instance a definition
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

  /** A denotation is the result of resolving
   *  a name (either simple identifier or select) during a given period.
   *
   *  Denotation has two subclasses: MultiDenotation and SingleDenotation.
   *
   *  A SingleDenotation refers to a `symbol` and a type (`info`) that the symbol has
   *  when seen from the reference.
   *
   *  Denotations can be combined with `&` and `|`.
   *  & is conjunction, | is disjunction.
   *
   *  `&` will create an overloaded denotation from two
   *  non-overloaded denotations if their signatures differ.
   *  Analogously `|` of two denotations with different signatures will give
   *  an empty denotation `NoDenotation`.
   *
   *  A denotation might refer to `NoSymbol`. This is the case if the denotation
   *  was produced from a disjunction of two denotations with different symbols
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
   *  Then the denotation of `y` is `SingleDenotation(NoSymbol, A | B)`.
   */
  abstract class Denotation extends DotClass {

    /** The referencing symbol, exists only for non-overloaded denotations */
    def symbol: Symbol

    /** The type info of the denotation, exists only for non-overloaded denotations */
    def info: Type

    /** The period during which this denotation is valid. */
    def validFor: Period

    /** Is this a reference to a type symbol? */
    def isType: Boolean

    /** Is this a reference to a term symbol? */
    def isTerm: Boolean = !isType

    /** Is this denotation overloaded? */
    def isOverloaded = isInstanceOf[MultiDenotation]

    /** The signature of the denotation */
    def signature(implicit ctx: Context): Signature

    /** Resolve overloaded denotation to pick the one with the given signature */
    def atSignature(sig: Signature)(implicit ctx: Context): SingleDenotation

    /** The variant of this denotation that's current in the given context. */
    def current(implicit ctx: Context): Denotation

    /** The first non-empty symbol, exists also for overloaded denotations,
     *  where an arbitrary variant's symbol is picked (with preference for existsing symbols) */
    def firstSym(implicit ctx: Context): Symbol

    def exists: Boolean = true

    def orElse(that: => Denotation) = if (this.exists) this else that

    /** The set of alternative single-denotations making up this denotation */
    def alternatives(implicit ctx: Context): List[SingleDenotation] =
      altsWith(scala.Function.const(true))

    /** The alternatives of this denotation that satisfy the predicate `p`. */
    def altsWith(p: Symbol => Boolean)(implicit ctx: Context): List[SingleDenotation]

    /** The unique alternative of this denotation that satisfies the predicate `p`,
     *  or NoDenotation if no satisfying alternative exists.
     *  @throws TypeError if there is at more than one alternative that satisfies `p`.
     */
    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation

    /** Does this denotation have an alternative that satisfies the predicate `p`? */
    def hasAltWith(p: Symbol => Boolean)(implicit ctx: Context): Boolean

    /** If this denotation is overloaded, filter with given predicate.
     *  If result is still overloaded throw a TypeError.
     *  Note: disambiguate is slightly different from suchThat in that
     *  single-denotations that do not satisfy the predicate are left alone
     *  (whereas suchThat would map them to NoDenotation).
     */
    def disambiguate(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation = this match {
      case sdenot: SingleDenotation => sdenot
      case mdenot => suchThat(p)
    }

    /** Return symbol in this denotation that satisfies the given predicate.
     *  Throw a `TypeError` if predicate fails to disambiguate symbol.
     *  Return a stubsymbol if no alternative satisfies the predicate.
     */
    def requiredSymbol(p: Symbol => Boolean, source: AbstractFile = null)(implicit ctx: Context): Symbol = {
      val sym = disambiguate(p).symbol
      if (sym.exists) sym else {
        val owner = if (firstSym.exists) firstSym.owner else NoSymbol
        ctx.newStubSymbol(owner, firstSym.name, source)
      }
    }

    /** Form a denotation by conjoining with denotation `that` */
    def &(that: Denotation)(implicit ctx: Context): Denotation =
      if (this eq that) this
      else if (!this.exists) that
      else if (!that.exists) this
      else that match {
        case that: SingleDenotation =>
          val r = mergeDenot(this, that)
          if (r ne NoDenotation) r else MultiDenotation(this, that)
        case that @ MultiDenotation(denot1, denot2) =>
          this & denot1 & denot2
      }

    /** Try to merge denot1 and denot2 without adding a new signature.
     *  If unsuccessful, return NoDenotation.
     */
    private def mergeDenot(denot1: Denotation, denot2: SingleDenotation)(implicit ctx: Context): Denotation = denot1 match {
      case denot1 @ MultiDenotation(denot11, denot12) =>
        val d1 = mergeDenot(denot11, denot2)
        if (d1 ne NoDenotation) d1 else mergeDenot(denot12, denot2)
      case denot1: SingleDenotation =>
        if (denot1 eq denot2) denot1
        else if (denot1.signature == denot2.signature) {
          def isEligible(sym1: Symbol, sym2: Symbol) =
            if (sym1.isType) !sym1.isClass
            else !(sym1 is Deferred) || (sym2 is Deferred) || !sym2.exists
          def normalize(info: Type) =
            if (isType) info.bounds else info
          val sym1 = denot1.symbol
          val info1 = denot1.info
          val sym2 = denot2.symbol
          val info2 = denot2.info
          val sym1Eligible = isEligible(sym1, sym2)
          val sym2Eligible = isEligible(sym2, sym1)
          val bounds1 = normalize(info1)
          val bounds2 = normalize(info2)
          if (sym2Eligible && bounds2 <:< bounds1) denot2
          else if (sym1Eligible && bounds1 <:< bounds2) denot1
          else new JointRefDenotation(
            if (sym2Eligible) sym2 else sym1,
            bounds1 & bounds2,
            denot1.validFor & denot2.validFor)
        } else NoDenotation
    }

    def |(that: Denotation)(pre: Type)(implicit ctx: Context): Denotation = {

      def lubSym(sym1: Symbol, sym2: Symbol): Symbol = {
        def qualifies(sym: Symbol) =
          sym.isAccessibleFrom(pre) && sym2.owner.isSubClass(sym.owner)
        sym1.allOverriddenSymbols findSymbol qualifies
      }

      def throwError = throw new MatchError(s"$this | $that")

      if (this eq that) this
      else if (!this.exists) this
      else if (!that.exists) that
      else this match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          denot1.derivedMultiDenotation((denot11 | that)(pre), (denot12 | that)(pre))
        case _ =>
          that match {
            case denot2 @ MultiDenotation(denot21, denot22) =>
              denot2.derivedMultiDenotation((this | denot21)(pre), (this | denot22)(pre))
            case denot2: SingleDenotation =>
              this match {
                case denot1: SingleDenotation =>
                  if (denot1.signature != denot2.signature) NoDenotation
                  else new JointRefDenotation(
                    lubSym(denot1.symbol, denot2.symbol),
                    denot1.info | denot2.info,
                    denot1.validFor & denot2.validFor)
                case _ =>
                  throwError
              }
            case _ =>
              throwError
          }
      }
    }

    def show(implicit ctx: Context): String = ctx.show(this)
  }

  /** The class of overloaded denotations
   *  @param  variants   The overloaded variants indexed by thheir signatures.
   */
  case class MultiDenotation(denot1: Denotation, denot2: Denotation) extends Denotation {
    final override def isType = false
    def derivedMultiDenotation(d1: Denotation, d2: Denotation) =
      if ((d1 eq denot1) && (d2 eq denot2)) this else MultiDenotation(d1, d2)
    def symbol = unsupported("symbol")
    def info = unsupported("info")
    def signature(implicit ctx: Context) = unsupported("signature")
    def firstSym(implicit ctx: Context): Symbol = denot1.firstSym orElse denot2.firstSym
    def altsWith(p: Symbol => Boolean)(implicit ctx: Context): List[SingleDenotation] =
      denot1.altsWith(p) ++ denot2.altsWith(p)
    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation = {
      val sd1 = denot1.suchThat(p)
      val sd2 = denot2.suchThat(p)
      if (sd1.exists)
        if (sd2.exists) throw new TypeError(s"failure to disambiguate overloaded reference $this")
        else sd1
      else sd2
    }
    def hasAltWith(p: Symbol => Boolean)(implicit ctx: Context): Boolean =
      denot1.hasAltWith(p) || denot2.hasAltWith(p)
    def atSignature(sig: Signature)(implicit ctx: Context): SingleDenotation =
      denot1.atSignature(sig) orElse denot2.atSignature(sig)
    def validFor = denot1.validFor & denot2.validFor
    def current(implicit ctx: Context): Denotation =
      derivedMultiDenotation(denot1.current, denot2.current)
  }

  abstract class SingleDenotation extends Denotation with DenotationSet {
    override def isType = info.isInstanceOf[TypeType]
    override def signature(implicit ctx: Context): Signature = {
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
    def firstSym(implicit ctx: Context): Symbol = symbol

    def derivedSingleDenotation(s: Symbol, i: Type): SingleDenotation =
      if ((s eq symbol) && (i eq info)) this else newLikeThis(s, i)

    protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = this

    def orElse(that: => SingleDenotation) = if (this.exists) this else that

    def altsWith(p: Symbol => Boolean)(implicit ctx: Context): List[SingleDenotation] =
      if (p(symbol)) this :: Nil else Nil

    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation =
      if (p(symbol)) this else NoDenotation

    def hasAltWith(p: Symbol => Boolean)(implicit ctx: Context): Boolean =
      p(symbol)

    def atSignature(sig: Signature)(implicit ctx: Context): SingleDenotation =
      if (sig == signature) this else NoDenotation

    // ------ Transformations -----------------------------------------

    private[this] var _validFor: Period = Nowhere

    def validFor = _validFor
    def validFor_=(p: Period) =
      _validFor = p

    /** The next SingleDenotation in this run, with wrap-around from last to first.
     *
     *  There may be several `SingleDenotation`s with different validity
     *  representing the same underlying definition at different phases.
     *  These are called a "flock". Flock members are generated by
     *  @See current. Flock members are connected in a ring
     *  with their `nextInRun` fields.
     *
     *  There are the following invariants converning flock members
     *
     *  1) validity periods must be non-overlapping
     *  2) the union of all validity periods must be a contiguous
     *     interval starting in FirstPhaseId.
     */
    var nextInRun: SingleDenotation = this

    /** The version of this SingleDenotation that was valid in the first phase
     *  of this run.
     */
    def initial: SingleDenotation = {
      var current = nextInRun
      while (current.validFor.code > this._validFor.code) current = current.nextInRun
      current
    }

    def current(implicit ctx: Context): SingleDenotation = {
      val currentPeriod = ctx.period
      val valid = _validFor
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
          // in this case, containsPeriod(next._validFor, currentPeriod)
          current = next
        } else {
          // not found, current points to highest existing variant
          var startPid = current.validFor.lastPhaseId + 1
          val trans = ctx.transformersFor(current)
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

    final def asSymDenotation = asInstanceOf[SymDenotation]

    // ------ DenotationSet ops ----------------------------------------------

    def first = this
    def toDenot(implicit ctx: Context) = this
    def containsSig(sig: Signature)(implicit ctx: Context) =
      signature == sig
    def filterDisjoint(denots: DenotationSet)(implicit ctx: Context): DenotationSet =
      if (denots.containsSig(signature)) NoDenotation else this
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): DenotationSet =
      if (symbol is flags) NoDenotation else this
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): DenotationSet =
      if (symbol.isAccessibleFrom(pre)) this else NoDenotation
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): DenotationSet =
      derivedSingleDenotation(symbol, info.asSeenFrom(pre, owner))
  }

  class UniqueRefDenotation(val symbol: Symbol,
    val info: Type,
    initValidFor: Period) extends SingleDenotation {
    validFor = initValidFor
    override protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)
  }

  class JointRefDenotation(val symbol: Symbol,
    val info: Type,
    initValidFor: Period) extends SingleDenotation {
    validFor = initValidFor
    override protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new JointRefDenotation(s, i, validFor)
  }

  class ErrorDenotation(implicit ctx: Context) extends SingleDenotation {
    val symbol = NoSymbol
    val info = NoType
    validFor = Period.allInRun(ctx.runId)
  }

  // --------------- DenotationSets -------------------------------------------------

  /** A DenotationSet represents a set of single denotations
   *  It is used as an optimization to avoid forming MultiDenotations too eagerly.
   */
  trait DenotationSet {
    def exists: Boolean
    def first: Denotation
    def toDenot(implicit ctx: Context): Denotation
    def containsSig(sig: Signature)(implicit ctx: Context): Boolean
    def filterDisjoint(denots: DenotationSet)(implicit ctx: Context): DenotationSet
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): DenotationSet
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): DenotationSet
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): DenotationSet
    def union(that: DenotationSet) =
      if (!this.exists) that
      else if (that.exists) this
      else DenotUnion(this, that)
  }

  case class DenotUnion(denots1: DenotationSet, denots2: DenotationSet) extends DenotationSet {
    assert(denots1.exists && denots2.exists)
    private def derivedUnion(s1: DenotationSet, s2: DenotationSet) =
      if (!s1.exists) s2
      else if (!s2.exists) s1
      else if ((s1 eq denots2) && (s2 eq denots2)) this
      else new DenotUnion(s1, s2)
    def exists = true
    def first = denots1.first
    def toDenot(implicit ctx: Context) = denots1.toDenot & denots2.toDenot
    def containsSig(sig: Signature)(implicit ctx: Context) =
      (denots1 containsSig sig) || (denots2 containsSig sig)
    //def filter(p: Symbol => Boolean)(implicit ctx: Context) =
    //  derivedUnion(denots1 filter p, denots2 filter p)
    def filterDisjoint(denots: DenotationSet)(implicit ctx: Context): DenotationSet =
      derivedUnion(denots1 filterDisjoint denots, denots2 filterDisjoint denots)
    def filterExcluded(flags: FlagSet)(implicit ctx: Context): DenotationSet =
      derivedUnion(denots1 filterExcluded flags, denots2 filterExcluded flags)
    def filterAccessibleFrom(pre: Type)(implicit ctx: Context): DenotationSet =
      derivedUnion(denots1 filterAccessibleFrom pre, denots2 filterAccessibleFrom pre)
    def asSeenFrom(pre: Type, owner: Symbol)(implicit ctx: Context): DenotationSet =
      derivedUnion(denots1.asSeenFrom(pre, owner), denots2.asSeenFrom(pre, owner))
  }

  /** Creation method for denotations */
  trait DenotationsBase { this: ContextBase =>
    def staticRef(path: Name)(implicit ctx: Context): Denotation = {
      def recur(path: Name, len: Int): Denotation = {
        val point = path.lastIndexOf('.', len - 1)
        val owner =
          if (point > 0) recur(path.toTermName, point).disambiguate(_.isParameterless)
          else if (path.isTermName) defn.RootClass.denot
          else defn.EmptyPackageClass.denot
        if (!owner.exists) owner
        else {
          val name = path slice (point + 1, len)
          val result = owner.info.member(name)
          if (result.exists) result
          else {
            val alt = missingHook(owner.symbol, name)
            if (alt.exists) alt.denot
            else result
          }
        }
      }
      recur(path, path.length)
    }

    def missingHook(owner: Symbol, name: Name)(implicit ctx: Context): Symbol =
      if (owner.isPackage && name.isTermName)
        ctx.newCompletePackageSymbol(owner, name.asTermName).entered
      else
        NoSymbol
  }
}

