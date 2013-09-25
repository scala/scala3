package dotty.tools
package dotc
package core

import SymDenotations.{ SymDenotation, NoDenotation }
import Contexts.{Context, ContextBase}
import Names.{Name, PreName}
import Names.TypeName
import Symbols.NoSymbol
import Symbols._
import Types._, Periods._, Flags._, Transformers._
import printing.Texts._
import printing.Printer
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
 *  Denotation-+-----SingleDenotation-+------SymDenotation-+----ClassDenotation
 *             |                      |
 *             +-----MultiDenotation  |
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
 *  ClassDenotation          A denotation representing a single class definition.
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
   *
   *  TODO: discriminate on result type as well !!!
   */
  type Signature = List[TypeName]

  /** The signature of a val or parameterless def, as opposed
   *  to List(), which is the signature of a zero-parameter def.
   */
  val NotAMethod: Signature = List(Names.EmptyTypeName)

  /** A denotation is the result of resolving
   *  a name (either simple identifier or select) during a given period.
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
   *    val x: A | B = if (test) new A else new B
   *    val y = x.f
   *
   *  Then the denotation of `y` is `SingleDenotation(NoSymbol, A | B)`.
   */
  abstract class Denotation extends DotClass with printing.Showable {

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

    /** Is this denotation different from NoDenotation or an ErrorDenotation? */
    def exists: Boolean = true

    /** If this denotation does not exist, fallback to alternative */
    def orElse(that: => Denotation) = if (this.exists) this else that

    /** The set of alternative single-denotations making up this denotation */
    def alternatives: List[SingleDenotation] =
      altsWith(scala.Function.const(true))

    /** The alternatives of this denotation that satisfy the predicate `p`. */
    def altsWith(p: Symbol => Boolean): List[SingleDenotation]

    /** The unique alternative of this denotation that satisfies the predicate `p`,
     *  or NoDenotation if no satisfying alternative exists.
     *  @throws TypeError if there is at more than one alternative that satisfies `p`.
     */
    def suchThat(p: Symbol => Boolean): SingleDenotation

    /** Does this denotation have an alternative that satisfies the predicate `p`? */
    def hasAltWith(p: SingleDenotation => Boolean): Boolean

    /** The denotation made up from the alternatives of this denotation that
     *  are accessible from prefix `pre`, or NoDenotation if no accessible alternative exists.
     */
    def accessibleFrom(pre: Type, superAccess: Boolean = false)(implicit ctx: Context): Denotation

    /** Find member of this denotation with given name and
     *  produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members that have
     *  flags in `excluded` from consideration.
     */
    def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation =
      info.findMember(name, pre, excluded)

    /** If this denotation is overloaded, filter with given predicate.
     *  If result is still overloaded throw a TypeError.
     *  Note: disambiguate is slightly different from suchThat in that
     *  single-denotations that do not satisfy the predicate are left alone
     *  (whereas suchThat would map them to NoDenotation).
     */
    def disambiguate(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation = this match {
      case sdenot: SingleDenotation => sdenot
      case mdenot => suchThat(p) orElse NoQualifyingRef(alternatives)
    }

    /** Return symbol in this denotation that satisfies the given predicate.
     *  Return a stubsymbol denotation is a missing ref.
     *  Throw a `TypeError` if predicate fails to disambiguate symbol or no alternative matches.
     */
    def requiredSymbol(p: Symbol => Boolean, source: AbstractFile = null)(implicit ctx: Context): Symbol =
      disambiguate(p) match {
        case MissingRef(ownerd, name) =>
          ctx.newStubSymbol(ownerd.symbol, name, source)
        case NoDenotation | _: NoQualifyingRef =>
          throw new TypeError(s"None of the alternatives of $this satisfies required predicate")
        case denot =>
          denot.symbol
      }

    def requiredMethod(name: PreName)(implicit ctx: Context): TermSymbol =
      info.member(name.toTermName).requiredSymbol(_ is Method).asTerm

    /** The denotation that has a type matching `targetType` when seen
     *  as a member of type `site`, `NoDenotation` if none exists.
     */
    def matchingDenotation(site: Type, targetType: Type)(implicit ctx: Context): SingleDenotation =
      if (isOverloaded)
        atSignature(targetType.signature).matchingDenotation(site, targetType)
      else if (exists && !(site.memberInfo(symbol) matches targetType))
        NoDenotation
      else
        this.asInstanceOf[SingleDenotation]

    /** Form a denotation by conjoining with denotation `that` */
    def & (that: Denotation, pre: Type)(implicit ctx: Context): Denotation = {

      /** Try to merge denot1 and denot2 without adding a new signature.
       *  If unsuccessful, return NoDenotation.
       */
      def mergeDenot(denot1: Denotation, denot2: SingleDenotation): Denotation = denot1 match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          val d1 = mergeDenot(denot11, denot2)
          if (d1.exists) denot1.derivedMultiDenotation(d1, denot2)
          else {
            val d2 = mergeDenot(denot12, denot2)
            if (d2.exists) denot1.derivedMultiDenotation(denot11, d2)
            else NoDenotation
          }
        case denot1: SingleDenotation =>
          if (denot1 eq denot2) denot1
          else if (denot1.signature != denot2.signature) NoDenotation
          else {
            val info1 = denot1.info
            val info2 = denot2.info
            val sym2 = denot2.symbol
            def sym2Accessible = sym2.isAccessibleFrom(pre)
            if (info2 <:< info1 && sym2Accessible) denot2
            else {
              val sym1 = denot1.symbol
              def sym1Accessible = sym1.isAccessibleFrom(pre)
              if (info1 <:< info2 && sym1Accessible) denot1
              else {
                val sym =
                  if (!sym1.exists) sym2
                  else if (!sym2.exists) sym1
                  else if (!sym1Accessible) sym2
                  else if (!sym2Accessible) sym1
                  else if (sym2 isAsConcrete sym1) sym2
                  else sym1
                new JointRefDenotation(sym, info1 & info2, denot1.validFor & denot2.validFor)
              }
            }
          }
      }

      if (this eq that) this
      else if (!this.exists) that
      else if (!that.exists) this
      else that match {
        case that: SingleDenotation =>
          val r = mergeDenot(this, that)
          if (r.exists) r else MultiDenotation(this, that)
        case that @ MultiDenotation(denot1, denot2) =>
          this & (denot1, pre) & (denot2, pre)
      }
    }

    /** Form a choice between this denotation and that one.
     *  @param pre  The prefix type of the members of the denotation, used
     *              to determine an accessible symbol if it exists.
     */
    def | (that: Denotation, pre: Type)(implicit ctx: Context): Denotation = {

      def unionDenot(denot1: SingleDenotation, denot2: SingleDenotation): Denotation =
        if (denot1.signature != denot2.signature) NoDenotation
        else {
          val info1 = denot1.info
          val info2 = denot2.info
          val sym2 = denot2.symbol
          def sym2Accessible = sym2.isAccessibleFrom(pre)
          if (info1 <:< info2 && sym2Accessible) denot2
          else {
            val sym1 = denot1.symbol
            def sym1Accessible = sym1.isAccessibleFrom(pre)
            if (info2 <:< info1 && sym1Accessible) denot1
            else {
              def lubSym(overrides: Iterator[Symbol], previous: Symbol): Symbol =
                if (!overrides.hasNext) previous
                else {
                  val candidate = overrides.next
                  if (sym2.owner.isSubClass(candidate.owner))
                    if (candidate isAccessibleFrom pre) candidate
                    else lubSym(overrides, previous orElse candidate)
                  else
                    lubSym(overrides, previous)
                }
              new JointRefDenotation(
                lubSym(sym1.allOverriddenSymbols, NoSymbol),
                info1 | info2,
                denot1.validFor & denot2.validFor)
            }
          }
        }

      def throwError = throw new MatchError(s"$this | $that")

      if (this eq that) this
      else if (!this.exists) this
      else if (!that.exists) that
      else this match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          denot1.derivedMultiDenotation(denot11 | (that, pre), denot12 | (that, pre))
        case _ =>
          that match {
            case denot2 @ MultiDenotation(denot21, denot22) =>
              denot2.derivedMultiDenotation(this | (denot21, pre), this | (denot22, pre))
            case denot2: SingleDenotation =>
              this match {
                case denot1: SingleDenotation =>
                  unionDenot(denot1, denot2)
                case _ =>
                  throwError
              }
            case _ =>
              throwError
          }
      }
    }

    def toText(printer: Printer): Text = printer.toText(this)
  }

  /** An overloaded denotation consisting of the alternatives of both given denotations.
   */
  case class MultiDenotation(denot1: Denotation, denot2: Denotation) extends Denotation {
    final def symbol: Symbol = NoSymbol
    final def info = multiHasNot("info")
    final def validFor = denot1.validFor & denot2.validFor
    final def isType = false
    def signature(implicit ctx: Context) = multiHasNot("signature")
    def atSignature(sig: Signature)(implicit ctx: Context): SingleDenotation =
      denot1.atSignature(sig) orElse denot2.atSignature(sig)
    def current(implicit ctx: Context): Denotation =
      derivedMultiDenotation(denot1.current, denot2.current)
    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      denot1.altsWith(p) ++ denot2.altsWith(p)
    def suchThat(p: Symbol => Boolean): SingleDenotation = {
      val sd1 = denot1.suchThat(p)
      val sd2 = denot2.suchThat(p)
      if (sd1.exists)
        if (sd2.exists) throw new TypeError(s"failure to disambiguate overloaded reference $this")
        else sd1
      else sd2
    }
    def hasAltWith(p: SingleDenotation => Boolean): Boolean =
      denot1.hasAltWith(p) || denot2.hasAltWith(p)
    def accessibleFrom(pre: Type, superAccess: Boolean)(implicit ctx: Context): Denotation = {
      val d1 = denot1 accessibleFrom (pre, superAccess)
      val d2 = denot2 accessibleFrom (pre, superAccess)
      if (!d1.exists) d2
      else if (!d2.exists) d1
      else derivedMultiDenotation(d1, d2)
    }
    def derivedMultiDenotation(d1: Denotation, d2: Denotation) =
      if ((d1 eq denot1) && (d2 eq denot2)) this else MultiDenotation(d1, d2)
    override def toString = alternatives.mkString(" <and> ")

    private def multiHasNot(op: String): Nothing =
      throw new UnsupportedOperationException(
        s"multi-denotation with alternatives $alternatives does not implement operation $op")
  }

  /** A non-overloaded denotation */
  abstract class SingleDenotation extends Denotation with PreDenotation {
    override def isType = info.isInstanceOf[TypeType]
    override def signature(implicit ctx: Context): Signature = {
      if (isType) NotAMethod
      else info match {
        case tp: PolyType =>
          tp.resultType match {
            case mt: MethodType => mt.signature
            case _ => List()
          }
        case mt: MethodType => mt.signature
        case _ => NotAMethod
      }
    }

    def derivedSingleDenotation(symbol: Symbol, info: Type): SingleDenotation =
      if ((symbol eq this.symbol) && (info eq this.info)) this
      else newLikeThis(symbol, info)

    protected def newLikeThis(symbol: Symbol, info: Type): SingleDenotation = this

    def orElse(that: => SingleDenotation) = if (this.exists) this else that

    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      if (p(symbol)) this :: Nil else Nil

    def suchThat(p: Symbol => Boolean): SingleDenotation =
      if (p(symbol)) this else NoDenotation

    def hasAltWith(p: SingleDenotation => Boolean): Boolean =
      p(this)

    def accessibleFrom(pre: Type, superAccess: Boolean)(implicit ctx: Context): Denotation =
      if (symbol isAccessibleFrom (pre, superAccess)) this else NoDenotation

    def atSignature(sig: Signature)(implicit ctx: Context): SingleDenotation =
      if (sig == signature) this else NoDenotation

    // ------ Transformations -----------------------------------------

    private[this] var myValidFor: Period = Nowhere

    def validFor = myValidFor
    def validFor_=(p: Period) =
      myValidFor = p

    /** The next SingleDenotation in this run, with wrap-around from last to first.
     *
     *  There may be several `SingleDenotation`s with different validity
     *  representing the same underlying definition at different phases.
     *  These are called a "flock". Flock members are generated by
     *  @See current. Flock members are connected in a ring
     *  with their `nextInRun` fields.
     *
     *  There are the following invariants concerning flock members
     *
     *  1) validity periods are non-overlapping
     *  2) the union of all validity periods is a contiguous
     *     interval.
     */
    var nextInRun: SingleDenotation = this

    /** The version of this SingleDenotation that was valid in the first phase
     *  of this run.
     */
    def initial: SingleDenotation = {
      var current = nextInRun
      while (current.validFor.code > this.myValidFor.code) current = current.nextInRun
      current
    }

    /** Produce a denotation that is valid for the given context.
     *  Usually called when !(validFor contains ctx.period)
     *  (even though this is not a precondition).
     *  If the runId of the context is the same as runId of this denotation,
     *  the right flock member is located, or, if it does not exist yet,
     *  created by invoking a transformer (@See Transformers).
     *  If the runId's differ, but this denotation is a SymDenotation
     *  and its toplevel owner class or module
     *  is still a member of its enclosing package, then the whole flock
     *  is brought forward to be valid in the new runId. Otherwise
     *  the symbol is stale, which constitutes an internal error.
     *  TODO: Ensure that a subclass is renewed whenever one of its parents is.
     */
    def current(implicit ctx: Context): SingleDenotation = {
      val currentPeriod = ctx.period
      val valid = myValidFor
      def bringForward(): SingleDenotation = this match {
        case denot: SymDenotation if ctx.stillValid(denot) =>
          var d: SingleDenotation = denot
          do {
            d.validFor = Period(currentPeriod.runId, d.validFor.firstPhaseId, d.validFor.lastPhaseId)
            d = d.nextInRun
          } while (d ne denot)
          initial.copyIfParentInvalid
        case _ =>
          throw new Error(s"stale symbol; $this, defined in run ${valid.runId} is referred to in run ${currentPeriod.runId}")
      }
      if (valid.runId != currentPeriod.runId) bringForward.current
      else {
        var cur = this
        if (currentPeriod.code > valid.code) {
          // search for containing period as long as nextInRun increases.
          var next = nextInRun
          while (next.validFor.code > valid.code &&
            !(next.validFor contains currentPeriod)) {
            cur = next
            next = next.nextInRun
          }
          if (next.validFor.code > valid.code) {
            // in this case, next.validFor contains currentPeriod
            cur = next
          } else {
            // not found, cur points to highest existing variant
            var startPid = cur.validFor.lastPhaseId + 1
            val transformers = ctx.transformersFor(cur)
            val transformer = transformers.nextTransformer(startPid)
            next = transformer.transform(cur).copyIfParentInvalid
            if (next eq cur)
              startPid = cur.validFor.firstPhaseId
            else {
              cur.nextInRun = next
              cur = next
            }
            cur.validFor = Period(
              currentPeriod.runId, startPid, transformer.lastPhaseId)
          }
        } else {
          // currentPeriod < valid; in this case a version must exist
          // but to be defensive we check for infinite loop anyway
          var cnt = 0
          while (!(cur.validFor contains currentPeriod)) {
            cur = cur.nextInRun
            cnt += 1
            assert(cnt <= MaxPossiblePhaseId)
          }
        }
        cur
      }
    }

    /** For ClassDenotations only:
     *  If caches influenced by parent classes are still valid, the denotation
     *  itself, otherwise a freshly initialized copy.
     */
    def copyIfParentInvalid(implicit ctx: Context): SingleDenotation = this

    final def asSymDenotation = asInstanceOf[SymDenotation]

    override def toString =
      if (symbol == NoSymbol) symbol.toString
      else s"<SingleDenotation of type $info>"

    // ------ PreDenotation ops ----------------------------------------------

    final def first = this
    final def toDenot(pre: Type)(implicit ctx: Context) = this
    final def containsSig(sig: Signature)(implicit ctx: Context) =
      exists && signature == sig
    final def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation =
      if (p(this)) this else NoDenotation
    final def filterDisjoint(denots: PreDenotation)(implicit ctx: Context): SingleDenotation =
      if (denots.containsSig(signature)) NoDenotation else this
    def disjointAsSeenFrom(denots: PreDenotation, pre: Type)(implicit ctx: Context): SingleDenotation =
      if (isType) filterDisjoint(denots).asSeenFrom(pre)
      else asSeenFrom(pre).filterDisjoint(denots)
    final def filterExcluded(excluded: FlagSet)(implicit ctx: Context): SingleDenotation =
      if (excluded.isEmpty) this
      else this match {
        case thisd: SymDenotation =>
          if (thisd is excluded) NoDenotation else this
        case _ =>
          if (symbol is excluded) NoDenotation else this
      }
    def asSeenFrom(pre: Type)(implicit ctx: Context): SingleDenotation = {
      val owner = this match {
        case thisd: SymDenotation => thisd.owner
        case _ => if (symbol.exists) symbol.owner else NoSymbol
      }
      if (!owner.membersNeedAsSeenFrom(pre)) this
      else derivedSingleDenotation(symbol, info.asSeenFrom(pre, owner))
    }
  }

  class UniqueRefDenotation(
    val symbol: Symbol,
    val info: Type,
    initValidFor: Period) extends SingleDenotation {
    validFor = initValidFor
    override protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)
  }

  class JointRefDenotation(
    val symbol: Symbol,
    val info: Type,
    initValidFor: Period) extends SingleDenotation {
    validFor = initValidFor
    override protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new JointRefDenotation(s, i, validFor)
  }

  class ErrorDenotation(implicit ctx: Context) extends SingleDenotation {
    override def exists = false
    val symbol = NoSymbol
    val info = NoType
    validFor = Period.allInRun(ctx.runId)
  }

  case class MissingRef(val owner: SingleDenotation, name: Name)(implicit ctx: Context) extends ErrorDenotation

  case class NoQualifyingRef(alts: List[SingleDenotation])(implicit ctx: Context) extends ErrorDenotation

  // --------------- PreDenotations -------------------------------------------------

  /** A PreDenotation represents a group of single denotations
   *  It is used as an optimization to avoid forming MultiDenotations too eagerly.
   */
  trait PreDenotation {

    /** A denotation in the group exists */
    def exists: Boolean

    /** First denotation in the group */
    def first: Denotation

    /** Convert to full denotation by &-ing all elements */
    def toDenot(pre: Type)(implicit ctx: Context): Denotation

    /** Group contains a denotation with given signature */
    def containsSig(sig: Signature)(implicit ctx: Context): Boolean

    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation

    /** Keep only those denotations in this group which have a signature
     *  that's not already defined by `denots`.
     */
    def filterDisjoint(denots: PreDenotation)(implicit ctx: Context): PreDenotation

    def disjointAsSeenFrom(denots: PreDenotation, pre: Type)(implicit ctx: Context): PreDenotation

    /** Keep only those denotations in this group whose flags do not intersect
     *  with `excluded`.
     */
    def filterExcluded(excluded: FlagSet)(implicit ctx: Context): PreDenotation

    /** The denotation with info(s) as seen from prefix type */
    def asSeenFrom(pre: Type)(implicit ctx: Context): PreDenotation

    /** The union of two groups. */
    def union(that: PreDenotation) =
      if (!this.exists) that
      else if (!that.exists) this
      else DenotUnion(this, that)
  }

  case class DenotUnion(denots1: PreDenotation, denots2: PreDenotation) extends PreDenotation {
    assert(denots1.exists && denots2.exists)
    def exists = true
    def first = denots1.first
    def toDenot(pre: Type)(implicit ctx: Context) = (denots1 toDenot pre) & (denots2 toDenot pre, pre)
    def containsSig(sig: Signature)(implicit ctx: Context) =
      (denots1 containsSig sig) || (denots2 containsSig sig)
    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation =
      derivedUnion(denots1 filterWithPredicate p, denots2 filterWithPredicate p)
    def filterDisjoint(denots: PreDenotation)(implicit ctx: Context): PreDenotation =
      derivedUnion(denots1 filterDisjoint denots, denots2 filterDisjoint denots)
    def disjointAsSeenFrom(denots: PreDenotation, pre: Type)(implicit ctx: Context): PreDenotation =
      derivedUnion(denots1.disjointAsSeenFrom(denots, pre), denots2.disjointAsSeenFrom(denots, pre))
    def filterExcluded(excluded: FlagSet)(implicit ctx: Context): PreDenotation =
      derivedUnion(denots1.filterExcluded(excluded), denots2.filterExcluded(excluded))
    def asSeenFrom(pre: Type)(implicit ctx: Context): PreDenotation =
      derivedUnion(denots1.asSeenFrom(pre), denots2.asSeenFrom(pre))
    private def derivedUnion(denots1: PreDenotation, denots2: PreDenotation) =
      if ((denots1 eq this.denots1) && (denots2 eq this.denots2)) this
      else denots1 union denots2
  }

  // --------------- Context Base Trait -------------------------------

  trait DenotationsBase { this: ContextBase =>

    /** The current denotation of the static reference given by path,
     *  or a MissingRef or NoQualifyingRef instance, if it does not exist.
     */
    def staticRef(path: Name)(implicit ctx: Context): Denotation = {
      def recur(path: Name, len: Int): Denotation = {
        val point = path.lastIndexOf('.', len - 1)
        val owner =
          if (point > 0) recur(path.toTermName, point).disambiguate(_.info.isParameterless)
          else if (path.isTermName) defn.RootClass.denot
          else defn.EmptyPackageClass.denot
        if (!owner.exists) owner
        else {
          val name = path slice (point + 1, len)
          val result = owner.info.member(name)
          if (result ne NoDenotation) result
          else {
            val alt = missingHook(owner.symbol.moduleClass, name)
            if (alt.exists) alt.denot
            else MissingRef(owner, name)
          }
        }
      }
      recur(path, path.length)
    }

    /** If we are looking for a non-existing term name in a package,
     *  assume it is a package for which we do not have a directory and
     *  enter it.
     */
    def missingHook(owner: Symbol, name: Name)(implicit ctx: Context): Symbol =
      if ((owner is Package) && name.isTermName)
        ctx.newCompletePackageSymbol(owner, name.asTermName).entered
      else
        NoSymbol
  }
}

