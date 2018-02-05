package dotty.tools
package dotc
package core

import SymDenotations.{ SymDenotation, ClassDenotation, NoDenotation }
import Contexts.{Context, ContextBase}
import Names._
import NameOps._
import NameKinds._
import StdNames._
import Symbols.NoSymbol
import Symbols._
import Types._
import Periods._
import Flags._
import DenotTransformers._
import Decorators._
import dotc.transform.Erasure
import printing.Texts._
import printing.Printer
import io.AbstractFile
import config.Config
import util.common._
import collection.mutable.ListBuffer
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
 *  NamedType
 *    |                                          Symbol---------ClassSymbol
 *    |                                            |                |
 *    | denot                                      | denot          | denot
 *    v                                            v                v
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
 *                           It has two subtypes: TermRef and TypeRef
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

  implicit def eqDenotation: Eq[Denotation, Denotation] = Eq

  /** A PreDenotation represents a group of single denotations or a single multi-denotation
   *  It is used as an optimization to avoid forming MultiDenotations too eagerly.
   */
  abstract class PreDenotation extends util.DotClass {

    /** A denotation in the group exists */
    def exists: Boolean

    /** First/last denotation in the group */
    def first: Denotation
    def last: Denotation

    /** Convert to full denotation by &-ing all elements */
    def toDenot(pre: Type)(implicit ctx: Context): Denotation

    /** Group contains a denotation that refers to given symbol */
    def containsSym(sym: Symbol): Boolean

    /** Group contains a denotation with the same signature as `other` */
    def matches(other: SingleDenotation)(implicit ctx: Context): Boolean

    /** Keep only those denotations in this group which satisfy predicate `p`. */
    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation

    /** Keep only those denotations in this group which have a signature
     *  that's not already defined by `denots`.
     */
    def filterDisjoint(denots: PreDenotation)(implicit ctx: Context): PreDenotation

    /** Keep only those inherited members M of this predenotation for which the following is true
     *   - M is not marked Private
     *   - If M has a unique symbol, it does not appear in `prevDenots`.
     *   - M's signature as seen from prefix `pre` does not appear in `ownDenots`
     *  Return the denotation as seen from `pre`.
     *  Called from SymDenotations.computeMember. There, `ownDenots` are the denotations found in
     *  the base class, which shadow any inherited denotations with the same signature.
     *  `prevDenots` are the denotations that are defined in the class or inherited from
     *  a base type which comes earlier in the linearization.
     */
    def mapInherited(ownDenots: PreDenotation, prevDenots: PreDenotation, pre: Type)(implicit ctx: Context): PreDenotation

    /** Keep only those denotations in this group whose flags do not intersect
     *  with `excluded`.
     */
    def filterExcluded(excluded: FlagSet)(implicit ctx: Context): PreDenotation

    private[this] var cachedPrefix: Type = _
    private[this] var cachedAsSeenFrom: AsSeenFromResult = _
    private[this] var validAsSeenFrom: Period = Nowhere

    type AsSeenFromResult <: PreDenotation

    /** The denotation with info(s) as seen from prefix type */
    final def asSeenFrom(pre: Type)(implicit ctx: Context): AsSeenFromResult =
      if (Config.cacheAsSeenFrom) {
        if ((cachedPrefix ne pre) || ctx.period != validAsSeenFrom) {
          cachedAsSeenFrom = computeAsSeenFrom(pre)
          cachedPrefix = pre
          validAsSeenFrom = ctx.period
        }
        cachedAsSeenFrom
      } else computeAsSeenFrom(pre)

    protected def computeAsSeenFrom(pre: Type)(implicit ctx: Context): AsSeenFromResult

    /** The union of two groups. */
    def union(that: PreDenotation): PreDenotation =
      if (!this.exists) that
      else if (!that.exists) this
      else DenotUnion(this, that)
  }

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
   *
   *  @param symbol  The referencing symbol, or NoSymbol is none exists
   */
  abstract class Denotation(val symbol: Symbol) extends PreDenotation with printing.Showable {

    type AsSeenFromResult <: Denotation

    /** The type info of the denotation, exists only for non-overloaded denotations */
    def info(implicit ctx: Context): Type

    /** The type info, or, if this is a SymDenotation where the symbol
     *  is not yet completed, the completer
     */
    def infoOrCompleter: Type

    /** The period during which this denotation is valid. */
    def validFor: Period

    /** Is this a reference to a type symbol? */
    def isType: Boolean

    /** Is this a reference to a term symbol? */
    def isTerm: Boolean = !isType

    /** Is this denotation overloaded? */
    final def isOverloaded = isInstanceOf[MultiDenotation]

    /** Denotation points to unique symbol; false for overloaded denotations
     * and JointRef denotations.
     */
    def hasUniqueSym: Boolean

    /** The name of the denotation */
    def name(implicit ctx: Context): Name

    /** The signature of the denotation. */
    def signature(implicit ctx: Context): Signature

    /** Resolve overloaded denotation to pick the ones with the given signature
     *  when seen from prefix `site`.
     *  @param relaxed  When true, consider only parameter signatures for a match.
     */
    def atSignature(sig: Signature, site: Type = NoPrefix, relaxed: Boolean = false)(implicit ctx: Context): Denotation

    /** The variant of this denotation that's current in the given context.
     *  If no such denotation exists, returns the denotation with each alternative
     *  at its first point of definition.
     */
    def current(implicit ctx: Context): Denotation

    /** Is this denotation different from NoDenotation or an ErrorDenotation? */
    def exists: Boolean = true

    /** A denotation with the info of this denotation transformed using `f` */
    def mapInfo(f: Type => Type)(implicit ctx: Context): Denotation

    /** If this denotation does not exist, fallback to alternative */
    final def orElse(that: => Denotation) = if (this.exists) this else that

    /** The set of alternative single-denotations making up this denotation */
    final def alternatives: List[SingleDenotation] = altsWith(alwaysTrue)

    /** The alternatives of this denotation that satisfy the predicate `p`. */
    def altsWith(p: Symbol => Boolean): List[SingleDenotation]

    /** The unique alternative of this denotation that satisfies the predicate `p`,
     *  or NoDenotation if no satisfying alternative exists.
     *  @throws TypeError if there is at more than one alternative that satisfies `p`.
     */
    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation

    /** If this is a SingleDenotation, return it, otherwise throw a TypeError */
    def checkUnique(implicit ctx: Context): SingleDenotation = suchThat(alwaysTrue)

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
     *  if generateStubs is specified, return a stubsymbol if denotation is a missing ref.
     *  Throw a `TypeError` if predicate fails to disambiguate symbol or no alternative matches.
     */
    def requiredSymbol(p: Symbol => Boolean, source: AbstractFile = null, generateStubs: Boolean = true)(implicit ctx: Context): Symbol =
      disambiguate(p) match {
        case m @ MissingRef(ownerd, name) =>
          if (generateStubs) {
            if (ctx.settings.YdebugMissingRefs.value) m.ex.printStackTrace()
            ctx.newStubSymbol(ownerd.symbol, name, source)
          }
          else NoSymbol
        case NoDenotation | _: NoQualifyingRef =>
          throw new TypeError(i"None of the alternatives of $this satisfies required predicate")
        case denot =>
          denot.symbol
      }

    def requiredMethod(name: PreName)(implicit ctx: Context): TermSymbol =
      info.member(name.toTermName).requiredSymbol(_ is Method).asTerm
    def requiredMethodRef(name: PreName)(implicit ctx: Context): TermRef =
      requiredMethod(name).termRef

    def requiredMethod(name: PreName, argTypes: List[Type])(implicit ctx: Context): TermSymbol = {
      info.member(name.toTermName).requiredSymbol { x =>
        (x is Method) && {
          x.info.paramInfoss match {
            case paramInfos :: Nil => paramInfos.corresponds(argTypes)(_ =:= _)
            case _ => false
          }
        }
      }.asTerm
    }
    def requiredMethodRef(name: PreName, argTypes: List[Type])(implicit ctx: Context): TermRef =
      requiredMethod(name, argTypes).termRef

    def requiredValue(name: PreName)(implicit ctx: Context): TermSymbol =
      info.member(name.toTermName).requiredSymbol(_.info.isParameterless).asTerm
    def requiredValueRef(name: PreName)(implicit ctx: Context): TermRef =
      requiredValue(name).termRef

    def requiredClass(name: PreName)(implicit ctx: Context): ClassSymbol =
      info.member(name.toTypeName).requiredSymbol(_.isClass).asClass

    def requiredType(name: PreName)(implicit ctx: Context): TypeSymbol =
      info.member(name.toTypeName).requiredSymbol(_.isType).asType

    /** The alternative of this denotation that has a type matching `targetType` when seen
     *  as a member of type `site`, `NoDenotation` if none exists.
     */
    def matchingDenotation(site: Type, targetType: Type)(implicit ctx: Context): SingleDenotation = {
      def qualifies(sym: Symbol) = site.memberInfo(sym).matchesLoosely(targetType)
      if (isOverloaded) {
        atSignature(targetType.signature, site, relaxed = true) match {
          case sd: SingleDenotation => sd.matchingDenotation(site, targetType)
          case md => md.suchThat(qualifies(_))
        }
      }
      else if (exists && !qualifies(symbol)) NoDenotation
      else asSingleDenotation
    }

    /** Handle merge conflict by throwing a `MergeError` exception */
    private def mergeConflict(tp1: Type, tp2: Type)(implicit ctx: Context): Type = {
      def showType(tp: Type) = tp match {
        case ClassInfo(_, cls, _, _, _) => cls.showLocated
        case bounds: TypeBounds => i"type bounds $bounds"
        case _ => tp.show
      }
      if (true) throw new MergeError(s"cannot merge ${showType(tp1)} with ${showType(tp2)}", tp1, tp2)
      else throw new Error(s"cannot merge ${showType(tp1)} with ${showType(tp2)}") // flip condition for debugging
    }

    /** Merge parameter names of lambda types. If names in corresponding positions match, keep them,
     *  otherwise generate new synthetic names.
     */
    private def mergeParamNames(tp1: LambdaType, tp2: LambdaType): List[tp1.ThisName] =
      (for ((name1, name2, idx) <- (tp1.paramNames, tp2.paramNames, tp1.paramNames.indices).zipped)
       yield if (name1 == name2) name1 else tp1.companion.syntheticParamName(idx)).toList

    /** Form a denotation by conjoining with denotation `that`.
     *
     *  NoDenotations are dropped. MultiDenotations are handled by merging
     *  parts with same signatures. SingleDenotations with equal signatures
     *  are joined as follows:
     *
     *  In a first step, consider only those denotations which have symbols
     *  that are accessible from prefix `pre`.
     *
     *  If there are several such denotations, try to pick one by applying the following
     *  three precedence rules in decreasing order of priority:
     *
     *  1. Prefer denotations with more specific infos.
     *  2. If infos are equally specific, prefer denotations with concrete symbols over denotations
     *     with abstract symbols.
     *  3. If infos are equally specific and symbols are equally concrete,
     *     prefer denotations with symbols defined in subclasses
     *     over denotations with symbols defined in proper superclasses.
     *
     *  If there is exactly one (preferred) accessible denotation, return it.
     *
     *  If there is no preferred accessible denotation, return a JointRefDenotation
     *  with one of the operand symbols (unspecified which one), and an info which
     *  is the intersection (using `&` or `safe_&` if `safeIntersection` is true)
     *  of the infos of the operand denotations.
     *
     *  If SingleDenotations with different signatures are joined, return NoDenotation.
     */
    def & (that: Denotation, pre: Type, safeIntersection: Boolean = false)(implicit ctx: Context): Denotation = {

      /** Normally, `tp1 & tp2`. Special cases for matching methods and classes, with
       *  the possibility of raising a merge error.
       */
      def infoMeet(tp1: Type, tp2: Type): Type = {
        if (tp1 eq tp2) tp1
        else tp1 match {
          case tp1: TypeBounds =>
            tp2 match {
              case tp2: TypeBounds => if (safeIntersection) tp1 safe_& tp2 else tp1 & tp2
              case tp2: ClassInfo if tp1 contains tp2 => tp2
              case _ => mergeConflict(tp1, tp2)
            }
          case tp1: ClassInfo =>
            tp2 match {
              case tp2: ClassInfo if tp1.cls eq tp2.cls => tp1.derivedClassInfo(tp1.prefix & tp2.prefix)
              case tp2: TypeBounds if tp2 contains tp1 => tp1
              case _ => mergeConflict(tp1, tp2)
            }
          case tp1: MethodOrPoly =>
            tp2 match {
              case tp2: MethodOrPoly =>
                // Two remedial strategies:
                //
                //  1. Prefer method types over poly types. This is necessary to handle
                //     overloaded definitions like the following
                //
                //        def ++ [B >: A](xs: C[B]): D[B]
                //        def ++ (xs: C[A]): D[A]
                //
                //     (Code like this is found in the collection strawman)
                //
                // 2. In the case of two method types or two polytypes with matching
                //    parameters and implicit status, merge corresppnding parameter
                //    and result types.
                if (tp1.isInstanceOf[PolyType] && tp2.isInstanceOf[MethodType]) tp2
                else if (tp2.isInstanceOf[PolyType] && tp1.isInstanceOf[MethodType]) tp1
                else if (ctx.typeComparer.matchingParams(tp1, tp2) &&
                         tp1.isImplicitMethod == tp2.isImplicitMethod)
                  tp1.derivedLambdaType(
                    mergeParamNames(tp1, tp2), tp1.paramInfos,
                    infoMeet(tp1.resultType, tp2.resultType.subst(tp2, tp1)))
                else mergeConflict(tp1, tp2)
              case _ =>
                mergeConflict(tp1, tp2)
            }
          case _ =>
            tp1 & tp2
        }
      }

      /** Try to merge denot1 and denot2 without adding a new signature. */
      def mergeDenot(denot1: Denotation, denot2: SingleDenotation): Denotation = denot1 match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          val d1 = mergeDenot(denot11, denot2)
          if (d1.exists) denot1.derivedUnionDenotation(d1, denot12)
          else {
            val d2 = mergeDenot(denot12, denot2)
            if (d2.exists) denot1.derivedUnionDenotation(denot11, d2)
            else NoDenotation
          }
        case denot1: SingleDenotation =>
          if (denot1 eq denot2) denot1
          else if (denot1.matches(denot2)) mergeSingleDenot(denot1, denot2)
          else NoDenotation
      }

      /** Try to merge single-denotations. */
      def mergeSingleDenot(denot1: SingleDenotation, denot2: SingleDenotation): SingleDenotation = {
        val info1 = denot1.info
        val info2 = denot2.info
        val sym1 = denot1.symbol
        val sym2 = denot2.symbol

        val sym2Accessible = sym2.isAccessibleFrom(pre)

        /** Does `sym1` come before `sym2` in the linearization of `pre`? */
        def precedes(sym1: Symbol, sym2: Symbol) = {
          def precedesIn(bcs: List[ClassSymbol]): Boolean = bcs match {
            case bc :: bcs1 => (sym1 eq bc) || !(sym2 eq bc) && precedesIn(bcs1)
            case Nil => true
          }
          (sym1 ne sym2) &&
            (sym1.derivesFrom(sym2) ||
              !sym2.derivesFrom(sym1) && precedesIn(pre.baseClasses))
        }

        /** Similar to SymDenotation#accessBoundary, but without the special cases. */
        def accessBoundary(sym: Symbol) =
          if (sym.is(Private)) sym.owner
          else sym.privateWithin.orElse(
            if (sym.is(Protected)) sym.owner.enclosingPackageClass
            else defn.RootClass)

        /** Establish a partial order "preference" order between symbols.
         *  Give preference to `sym1` over `sym2` if one of the following
         *  conditions holds, in decreasing order of weight:
         *   1. sym2 doesn't exist
         *   2. sym1 is concrete and sym2 is abstract
         *   3. The owner of sym1 comes before the owner of sym2 in the linearization
         *      of the type of the prefix `pre`.
         *   4. The access boundary of sym2 is properly contained in the access
         *      boundary of sym1. For protected access, we count the enclosing
         *      package as access boundary.
         *   5. sym1 a method but sym2 is not.
         *  The aim of these criteria is to give some disambiguation on access which
         *   - does not depend on textual order or other arbitrary choices
         *   - minimizes raising of doubleDef errors
         */
        def preferSym(sym1: Symbol, sym2: Symbol) =
          sym1.eq(sym2) ||
          sym1.exists &&
            (!sym2.exists ||
              sym1.isAsConcrete(sym2) &&
              (!sym2.isAsConcrete(sym1) ||
                precedes(sym1.owner, sym2.owner) ||
                accessBoundary(sym2).isProperlyContainedIn(accessBoundary(sym1)) ||
                sym1.is(Method) && !sym2.is(Method)) ||
              sym1.info.isErroneous)

        /** Sym preference provided types also override */
        def prefer(sym1: Symbol, sym2: Symbol, info1: Type, info2: Type) =
          preferSym(sym1, sym2) &&
          info1.overrides(info2, sym1.matchNullaryLoosely || sym2.matchNullaryLoosely)

        def handleDoubleDef =
          if (preferSym(sym1, sym2)) denot1
          else if (preferSym(sym2, sym1)) denot2
          else doubleDefError(denot1, denot2, pre)

        if (sym2Accessible && prefer(sym2, sym1, info2, info1)) denot2
        else {
          val sym1Accessible = sym1.isAccessibleFrom(pre)
          if (sym1Accessible && prefer(sym1, sym2, info1, info2)) denot1
          else if (sym1Accessible && sym2.exists && !sym2Accessible) denot1
          else if (sym2Accessible && sym1.exists && !sym1Accessible) denot2
          else if (isDoubleDef(sym1, sym2)) handleDoubleDef
          else {
            val sym =
              if (preferSym(sym2, sym1)) sym2
              else sym1
            val jointInfo =
              try infoMeet(info1, info2)
              catch {
                case ex: MergeError =>
                  if (pre.widen.classSymbol.is(Scala2x) || ctx.scala2Mode)
                    info1 // follow Scala2 linearization -
                  // compare with way merge is performed in SymDenotation#computeMembersNamed
                  else
                    throw new MergeError(s"${ex.getMessage} as members of type ${pre.show}", ex.tp1, ex.tp2)
              }
            new JointRefDenotation(sym, jointInfo, denot1.validFor & denot2.validFor)
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

      /** Normally, `tp1 | tp2`. Special cases for matching methods and classes, with
       *  the possibility of raising a merge error.
       */
      def infoJoin(tp1: Type, tp2: Type): Type = tp1 match {
        case tp1: TypeBounds =>
          tp2 match {
            case tp2: TypeBounds => tp1 | tp2
            case tp2: ClassInfo if tp1 contains tp2 => tp1
            case _ => mergeConflict(tp1, tp2)
          }
        case tp1: ClassInfo =>
          tp2 match {
            case tp2: ClassInfo if tp1.cls eq tp2.cls => tp1.derivedClassInfo(tp1.prefix | tp2.prefix)
            case tp2: TypeBounds if tp2 contains tp1 => tp2
            case _ => mergeConflict(tp1, tp2)
          }
        case tp1: MethodOrPoly =>
          tp2 match {
            case tp2: MethodOrPoly
            if ctx.typeComparer.matchingParams(tp1, tp2) &&
               tp1.isImplicitMethod == tp2.isImplicitMethod =>
              tp1.derivedLambdaType(
                mergeParamNames(tp1, tp2), tp1.paramInfos,
                tp1.resultType | tp2.resultType.subst(tp2, tp1))
            case _ =>
              mergeConflict(tp1, tp2)
          }
        case _ =>
          tp1 | tp2
      }

      def unionDenot(denot1: SingleDenotation, denot2: SingleDenotation): Denotation =
        if (denot1.matches(denot2)) {
          val sym1 = denot1.symbol
          val sym2 = denot2.symbol
          val info1 = denot1.info
          val info2 = denot2.info
          val sameSym = sym1 eq sym2
          if (sameSym && (info1 frozen_<:< info2)) denot2
          else if (sameSym && (info2 frozen_<:< info1)) denot1
          else {
            val jointSym =
              if (sameSym) sym1
              else {
                val owner2 = if (sym2 ne NoSymbol) sym2.owner else NoSymbol
                /** Determine a symbol which is overridden by both sym1 and sym2.
                 *  Preference is given to accessible symbols.
                 */
                def lubSym(overrides: Iterator[Symbol], previous: Symbol): Symbol =
                  if (!overrides.hasNext) previous
                  else {
                    val candidate = overrides.next()
                    if (owner2 derivesFrom candidate.owner)
                      if (candidate isAccessibleFrom pre) candidate
                      else lubSym(overrides, previous orElse candidate)
                    else
                      lubSym(overrides, previous)
                  }
                lubSym(sym1.allOverriddenSymbols, NoSymbol)
              }
            new JointRefDenotation(
                jointSym, infoJoin(info1, info2), denot1.validFor & denot2.validFor)
          }
        }
        else NoDenotation

      if (this eq that) this
      else if (!this.exists) this
      else if (!that.exists) that
      else this match {
        case denot1 @ MultiDenotation(denot11, denot12) =>
          denot1.derivedUnionDenotation(denot11 | (that, pre), denot12 | (that, pre))
        case denot1: SingleDenotation =>
          that match {
            case denot2 @ MultiDenotation(denot21, denot22) =>
              denot2.derivedUnionDenotation(this | (denot21, pre), this | (denot22, pre))
            case denot2: SingleDenotation =>
              unionDenot(denot1, denot2)
          }
      }
    }

    final def asSingleDenotation = asInstanceOf[SingleDenotation]
    final def asSymDenotation = asInstanceOf[SymDenotation]

    def toText(printer: Printer): Text = printer.toText(this)

    // ------ PreDenotation ops ----------------------------------------------

    final def toDenot(pre: Type)(implicit ctx: Context): Denotation = this
    final def containsSym(sym: Symbol): Boolean = hasUniqueSym && (symbol eq sym)
  }

  /** A non-overloaded denotation */
  abstract class SingleDenotation(symbol: Symbol) extends Denotation(symbol) {
    protected def newLikeThis(symbol: Symbol, info: Type): SingleDenotation

    final def name(implicit ctx: Context): Name = symbol.name

    final def signature(implicit ctx: Context): Signature =
      if (isType) Signature.NotAMethod // don't force info if this is a type SymDenotation
      else info match {
        case info: MethodicType =>
          try info.signature
          catch { // !!! DEBUG
            case scala.util.control.NonFatal(ex) =>
              ctx.echo(s"cannot take signature of ${info.show}")
              throw ex
          }
        case _ => Signature.NotAMethod
      }

    def derivedSingleDenotation(symbol: Symbol, info: Type)(implicit ctx: Context): SingleDenotation =
      if ((symbol eq this.symbol) && (info eq this.info)) this
      else newLikeThis(symbol, info)

    def mapInfo(f: Type => Type)(implicit ctx: Context): SingleDenotation =
      derivedSingleDenotation(symbol, f(info))

    def orElse(that: => SingleDenotation) = if (this.exists) this else that

    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      if (exists && p(symbol)) this :: Nil else Nil

    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation =
      if (exists && p(symbol)) this else NoDenotation

    def hasAltWith(p: SingleDenotation => Boolean): Boolean =
      exists && p(this)

    def accessibleFrom(pre: Type, superAccess: Boolean)(implicit ctx: Context): Denotation =
      if (!symbol.exists || symbol.isAccessibleFrom(pre, superAccess)) this else NoDenotation

    def atSignature(sig: Signature, site: Type, relaxed: Boolean)(implicit ctx: Context): SingleDenotation = {
      val situated = if (site == NoPrefix) this else asSeenFrom(site)
      val matches = sig.matchDegree(situated.signature) >=
        (if (relaxed) Signature.ParamMatch else Signature.FullMatch)
      if (matches) this else NoDenotation
    }

    // ------ Forming types -------------------------------------------

    /** The TypeRef representing this type denotation at its original location. */
    def typeRef(implicit ctx: Context): TypeRef =
      TypeRef(symbol.owner.thisType, symbol.name.asTypeName, this)

    /** The typeRef applied to its own type parameters */
    def appliedRef(implicit ctx: Context): Type =
      typeRef.appliedTo(symbol.typeParams.map(_.typeRef))

    /** The TermRef representing this term denotation at its original location. */
    def termRef(implicit ctx: Context): TermRef =
      TermRef(symbol.owner.thisType, symbol.name.asTermName, this)

    /** The NamedType representing this denotation at its original location.
     *  Same as either `typeRef` or `termRef` depending whether this denotes a type or not.
     */
    def namedType(implicit ctx: Context): NamedType =
      if (isType) typeRef else termRef

    // ------ Transformations -----------------------------------------

    private[this] var myValidFor: Period = Nowhere

    def validFor = myValidFor
    def validFor_=(p: Period) = {
      myValidFor = p
      symbol.invalidateDenotCache()
    }

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
    protected var nextInRun: SingleDenotation = this

    /** The version of this SingleDenotation that was valid in the first phase
     *  of this run.
     */
    def initial: SingleDenotation =
      if (validFor.firstPhaseId <= 1) this
      else {
        var current = nextInRun
        while (current.validFor.code > this.myValidFor.code) current = current.nextInRun
        current
      }

    def history: List[SingleDenotation] = {
      val b = new ListBuffer[SingleDenotation]
      var current = initial
      do {
        b += (current)
        current = current.nextInRun
      }
      while (current ne initial)
      b.toList
    }

    /** Invalidate all caches and fields that depend on base classes and their contents */
    def invalidateInheritedInfo(): Unit = ()

    private def updateValidity()(implicit ctx: Context): this.type = {
      assert(ctx.runId >= validFor.runId || ctx.settings.YtestPickler.value, // mixing test pickler with debug printing can travel back in time
          s"denotation $this invalid in run ${ctx.runId}. ValidFor: $validFor")
      var d: SingleDenotation = this
      do {
        d.validFor = Period(ctx.period.runId, d.validFor.firstPhaseId, d.validFor.lastPhaseId)
        d.invalidateInheritedInfo()
        d = d.nextInRun
      } while (d ne this)
      this
    }

    /** Move validity period of this denotation to a new run. Throw a StaleSymbol error
     *  if denotation is no longer valid.
     *  However, StaleSymbol error is not thrown in the following situations:
     *
     *   - If ctx.acceptStale returns true (e.g. because we are in the IDE),
     *     update the symbol to the new version if it exists, or return
     *     the old version otherwise.
     *   - If the symbol did not have a denotation that was defined at the current phase
     *     return a NoDenotation instead.
     */
    private def bringForward()(implicit ctx: Context): SingleDenotation = {
      this match {
        case symd: SymDenotation =>
          if (ctx.stillValid(symd)) return updateValidity()
          if (ctx.acceptStale(symd)) {
            val newd = symd.owner.info.decls.lookup(symd.name)
            return (newd.denot: SingleDenotation).orElse(symd).updateValidity()
          }
        case _ =>
      }
      if (!symbol.exists) return updateValidity()
      if (!coveredInterval.containsPhaseId(ctx.phaseId)) return NoDenotation
      if (ctx.debug) ctx.traceInvalid(this)
      staleSymbolError
    }

    /** The next defined denotation (following `nextInRun`) or an arbitrary
     *  undefined denotation, if all denotations in a `nextinRun` cycle are
     *  undefined.
     */
    private def nextDefined: SingleDenotation = {
      var p1 = this
      var p2 = nextInRun
      while (p1.validFor == Nowhere && (p1 ne p2)) {
        p1 = p1.nextInRun
        p2 = p2.nextInRun.nextInRun
      }
      p1
    }

    /** Skip any denotations that have been removed by an installAfter or that
     *  are otherwise undefined.
     */
    def skipRemoved(implicit ctx: Context): SingleDenotation =
      if (myValidFor.code <= 0) nextDefined else this

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
     */
    def current(implicit ctx: Context): SingleDenotation = {
      val currentPeriod = ctx.period
      val valid = myValidFor
      if (valid.code <= 0) {
        // can happen if we sit on a stale denotation which has been replaced
        // wholesale by an installAfter; in this case, proceed to the next
        // denotation and try again.
        val nxt = nextDefined
        if (nxt.validFor != Nowhere) return nxt
        assert(false, this)
      }

      if (valid.runId != currentPeriod.runId)
        if (exists) initial.bringForward().current
        else this
      else {
        var cur = this
        if (currentPeriod.code > valid.code) {
          // search for containing period as long as nextInRun increases.
          var next = nextInRun
          while (next.validFor.code > valid.code && !(next.validFor contains currentPeriod)) {
            cur = next
            next = next.nextInRun
          }
          if (next.validFor.code > valid.code) {
            // in this case, next.validFor contains currentPeriod
            cur = next
            cur
          } else {
            //println(s"might need new denot for $cur, valid for ${cur.validFor} at $currentPeriod")
            // not found, cur points to highest existing variant
            val nextTransformerId = ctx.nextDenotTransformerId(cur.validFor.lastPhaseId)
            if (currentPeriod.lastPhaseId <= nextTransformerId)
              cur.validFor = Period(currentPeriod.runId, cur.validFor.firstPhaseId, nextTransformerId)
            else {
              var startPid = nextTransformerId + 1
              val transformer = ctx.denotTransformers(nextTransformerId)
              //println(s"transforming $this with $transformer")
              try {
                next = transformer.transform(cur)(ctx.withPhase(transformer))
              } catch {
                case ex: CyclicReference =>
                  println(s"error while transforming $this") // DEBUG
                  throw ex
              }
              if (next eq cur)
                startPid = cur.validFor.firstPhaseId
              else {
                next match {
                  case next: ClassDenotation =>
                    assert(!next.is(Package), s"illegal transformation of package denotation by transformer ${ctx.withPhase(transformer).phase}")
                  case _ =>
                }
                next.insertAfter(cur)
                cur = next
              }
              cur.validFor = Period(currentPeriod.runId, startPid, transformer.lastPhaseId)
              //printPeriods(cur)
              //println(s"new denot: $cur, valid for ${cur.validFor}")
            }
            cur.current // multiple transformations could be required
          }
        } else {
          // currentPeriod < end of valid; in this case a version must exist
          // but to be defensive we check for infinite loop anyway
          var cnt = 0
          while (!(cur.validFor contains currentPeriod)) {
            //println(s"searching: $cur at $currentPeriod, valid for ${cur.validFor}")
            cur = cur.nextInRun
            // Note: One might be tempted to add a `prev` field to get to the new denotation
            // more directly here. I tried that, but it degrades rather than improves
            // performance: Test setup: Compile everything in dotc and immediate subdirectories
            // 10 times. Best out of 10: 18154ms with `prev` field, 17777ms without.
            cnt += 1
            if (cnt > MaxPossiblePhaseId)
              return current(ctx.withPhase(coveredInterval.firstPhaseId))
          }
          cur
        }
      }
    }

    private def demandOutsideDefinedMsg(implicit ctx: Context): String =
      s"demanding denotation of $this at phase ${ctx.phase}(${ctx.phaseId}) outside defined interval: defined periods are${definedPeriodsString}"

    /** Install this denotation to be the result of the given denotation transformer.
     *  This is the implementation of the same-named method in SymDenotations.
     *  It's placed here because it needs access to private fields of SingleDenotation.
     *  @pre  Can only be called in `phase.next`.
     */
    protected def installAfter(phase: DenotTransformer)(implicit ctx: Context): Unit = {
      val targetId = phase.next.id
      if (ctx.phaseId != targetId) installAfter(phase)(ctx.withPhase(phase.next))
      else {
        val current = symbol.current
        // println(s"installing $this after $phase/${phase.id}, valid = ${current.validFor}")
        // printPeriods(current)
        this.validFor = Period(ctx.runId, targetId, current.validFor.lastPhaseId)
        if (current.validFor.firstPhaseId >= targetId)
          current.replaceWith(this)
        else {
          current.validFor = Period(ctx.runId, current.validFor.firstPhaseId, targetId - 1)
          insertAfter(current)
        }
      // printPeriods(this)
      }
    }

    /** Apply a transformation `f` to all denotations in this group that start at or after
     *  given phase. Denotations are replaced while keeping the same validity periods.
     */
    protected def transformAfter(phase: DenotTransformer, f: SymDenotation => SymDenotation)(implicit ctx: Context): Unit = {
      var current = symbol.current
      while (current.validFor.firstPhaseId < phase.id && (current.nextInRun.validFor.code > current.validFor.code))
        current = current.nextInRun
      var hasNext = true
      while ((current.validFor.firstPhaseId >= phase.id) && hasNext) {
        val current1: SingleDenotation = f(current.asSymDenotation)
        if (current1 ne current) {
          current1.validFor = current.validFor
          current.replaceWith(current1)
        }
        hasNext = current1.nextInRun.validFor.code > current1.validFor.code
        current = current1.nextInRun
      }
    }

    /** Insert this denotation so that it follows `prev`. */
    private def insertAfter(prev: SingleDenotation) = {
      this.nextInRun = prev.nextInRun
      prev.nextInRun = this
    }

    /** Insert this denotation instead of `old`.
     *  Also ensure that `old` refers with `nextInRun` to this denotation
     *  and set its `validFor` field to `NoWhere`. This is necessary so that
     *  references to the old denotation can be brought forward via `current`
     *  to a valid denotation.
     *
     *  The code to achieve this is subtle in that it works correctly
     *  whether the replaced denotation is the only one in its cycle or not.
     */
    private[dotc] def replaceWith(newd: SingleDenotation): Unit = {
      var prev = this
      while (prev.nextInRun ne this) prev = prev.nextInRun
      // order of next two assignments is important!
      prev.nextInRun = newd
      newd.nextInRun = nextInRun
      validFor = Nowhere
      nextInRun = newd
    }

    def staleSymbolError(implicit ctx: Context) =
      throw new StaleSymbol(staleSymbolMsg)

    def staleSymbolMsg(implicit ctx: Context): String = {
      def ownerMsg = this match {
        case denot: SymDenotation => s"in ${denot.owner}"
        case _ => ""
      }
      s"stale symbol; $this#${symbol.id} $ownerMsg, defined in ${myValidFor}, is referred to in run ${ctx.period}"
    }

    /** The period (interval of phases) for which there exists
     *  a valid denotation in this flock.
     */
    def coveredInterval(implicit ctx: Context): Period = {
      var cur = this
      var cnt = 0
      var interval = validFor
      do {
        cur = cur.nextInRun
        cnt += 1
        assert(cnt <= MaxPossiblePhaseId, demandOutsideDefinedMsg)
        interval |= cur.validFor
      } while (cur ne this)
      interval
    }

    /** For ClassDenotations only:
     *  If caches influenced by parent classes are still valid, the denotation
     *  itself, otherwise a freshly initialized copy.
     */
    def syncWithParents(implicit ctx: Context): SingleDenotation = this

    /** Show declaration string; useful for showing declarations
     *  as seen from subclasses.
     */
    def showDcl(implicit ctx: Context): String = ctx.dclText(this).show

    override def toString =
      if (symbol == NoSymbol) symbol.toString
      else s"<SingleDenotation of type $infoOrCompleter>"

    def definedPeriodsString: String = {
      var sb = new StringBuilder()
      var cur = this
      var cnt = 0
      do {
        sb.append(" " + cur.validFor)
        cur = cur.nextInRun
        cnt += 1
        if (cnt > MaxPossiblePhaseId) { sb.append(" ..."); cur = this }
      } while (cur ne this)
      sb.toString
    }

    // ------ PreDenotation ops ----------------------------------------------

    final def first = this
    final def last = this

    final def matches(other: SingleDenotation)(implicit ctx: Context): Boolean = {
      val d = signature.matchDegree(other.signature)
      d == Signature.FullMatch ||
      d >= Signature.ParamMatch && info.matches(other.info)
    }

    final def filterWithPredicate(p: SingleDenotation => Boolean): SingleDenotation =
      if (p(this)) this else NoDenotation
    final def filterDisjoint(denots: PreDenotation)(implicit ctx: Context): SingleDenotation =
      if (denots.exists && denots.matches(this)) NoDenotation else this
    def mapInherited(ownDenots: PreDenotation, prevDenots: PreDenotation, pre: Type)(implicit ctx: Context): SingleDenotation =
      if (hasUniqueSym && prevDenots.containsSym(symbol)) NoDenotation
      else if (isType) filterDisjoint(ownDenots).asSeenFrom(pre)
      else asSeenFrom(pre).filterDisjoint(ownDenots)
    final def filterExcluded(excluded: FlagSet)(implicit ctx: Context): SingleDenotation =
      if (excluded.isEmpty || !(this overlaps excluded)) this else NoDenotation

    type AsSeenFromResult = SingleDenotation
    protected def computeAsSeenFrom(pre: Type)(implicit ctx: Context): SingleDenotation = {
      val symbol = this.symbol
      val owner = this match {
        case thisd: SymDenotation => thisd.owner
        case _ => if (symbol.exists) symbol.owner else NoSymbol
      }
      if (!owner.membersNeedAsSeenFrom(pre) || symbol.is(NonMember)) this
      else derivedSingleDenotation(symbol, symbol.info.asSeenFrom(pre, owner))
    }

    private def overlaps(fs: FlagSet)(implicit ctx: Context): Boolean = this match {
      case sd: SymDenotation => sd is fs
      case _ => symbol is fs
    }
  }

  abstract class NonSymSingleDenotation(symbol: Symbol) extends SingleDenotation(symbol) {
    def infoOrCompleter: Type
    def info(implicit ctx: Context) = infoOrCompleter
    def isType = infoOrCompleter.isInstanceOf[TypeType]
  }

  class UniqueRefDenotation(
    symbol: Symbol,
    val infoOrCompleter: Type,
    initValidFor: Period) extends NonSymSingleDenotation(symbol) {
    validFor = initValidFor
    override def hasUniqueSym: Boolean = true
    protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)
  }

  class JointRefDenotation(
    symbol: Symbol,
    val infoOrCompleter: Type,
    initValidFor: Period) extends NonSymSingleDenotation(symbol) {
    validFor = initValidFor
    override def hasUniqueSym = false
    protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = new JointRefDenotation(s, i, validFor)
  }

  class ErrorDenotation(implicit ctx: Context) extends NonSymSingleDenotation(NoSymbol) {
    override def exists = false
    override def hasUniqueSym = false
    def infoOrCompleter = NoType
    validFor = Period.allInRun(ctx.runId)
    protected def newLikeThis(s: Symbol, i: Type): SingleDenotation = this
  }

  /** An error denotation that provides more info about the missing reference.
   *  Produced by staticRef, consumed by requiredSymbol.
   */
  case class MissingRef(val owner: SingleDenotation, name: Name)(implicit ctx: Context) extends ErrorDenotation {
    val ex: Exception = new Exception // DEBUG
  }

  /** An error denotation that provides more info about alternatives
   *  that were found but that do not qualify.
   *  Produced by staticRef, consumed by requiredSymbol.
   */
  case class NoQualifyingRef(alts: List[SingleDenotation])(implicit ctx: Context) extends ErrorDenotation

  /** A double definition
   */
  def isDoubleDef(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean =
    (sym1.exists && sym2.exists &&
    (sym1 ne sym2) && (sym1.owner eq sym2.owner) &&
    !sym1.is(Bridge) && !sym2.is(Bridge))

  def doubleDefError(denot1: Denotation, denot2: Denotation, pre: Type = NoPrefix)(implicit ctx: Context): Nothing = {
    val sym1 = denot1.symbol
    val sym2 = denot2.symbol
    def fromWhere = if (pre == NoPrefix) "" else i"\nwhen seen as members of $pre"
    val msg =
      if (denot1.isTerm)
        i"""cannot merge
           |  $sym1: ${sym1.info}  and
           |  $sym2: ${sym2.info};
           |they are both defined in ${sym1.owner} but have matching signatures
           |  ${denot1.info} and
           |  ${denot2.info}$fromWhere"""
      else
        i"""cannot merge
           |  $sym1 ${denot1.info}
           |  $sym2 ${denot2.info}
           |they are conflicting definitions$fromWhere"""
    throw new MergeError(msg, denot2.info, denot2.info)
  }

  // --- Overloaded denotations and predenotations -------------------------------------------------

  trait MultiPreDenotation extends PreDenotation {
    def denot1: PreDenotation
    def denot2: PreDenotation

    assert(denot1.exists && denot2.exists, s"Union of non-existing denotations ($denot1) and ($denot2)")
    def first = denot1.first
    def last = denot2.last
    def matches(other: SingleDenotation)(implicit ctx: Context): Boolean =
      denot1.matches(other) || denot2.matches(other)
    def filterWithPredicate(p: SingleDenotation => Boolean): PreDenotation =
      derivedUnion(denot1 filterWithPredicate p, denot2 filterWithPredicate p)
    def filterDisjoint(denot: PreDenotation)(implicit ctx: Context): PreDenotation =
      derivedUnion(denot1 filterDisjoint denot, denot2 filterDisjoint denot)
    def mapInherited(owndenot: PreDenotation, prevdenot: PreDenotation, pre: Type)(implicit ctx: Context): PreDenotation =
      derivedUnion(denot1.mapInherited(owndenot, prevdenot, pre), denot2.mapInherited(owndenot, prevdenot, pre))
    def filterExcluded(excluded: FlagSet)(implicit ctx: Context): PreDenotation =
      derivedUnion(denot1.filterExcluded(excluded), denot2.filterExcluded(excluded))
    protected def derivedUnion(denot1: PreDenotation, denot2: PreDenotation) =
      if ((denot1 eq this.denot1) && (denot2 eq this.denot2)) this
      else denot1 union denot2
  }

  final case class DenotUnion(denot1: PreDenotation, denot2: PreDenotation) extends MultiPreDenotation {
    def exists = true
    def toDenot(pre: Type)(implicit ctx: Context) =
      (denot1 toDenot pre) & (denot2 toDenot pre, pre)
    def containsSym(sym: Symbol) =
      (denot1 containsSym sym) || (denot2 containsSym sym)
    type AsSeenFromResult = PreDenotation
    def computeAsSeenFrom(pre: Type)(implicit ctx: Context): PreDenotation =
      derivedUnion(denot1.asSeenFrom(pre), denot2.asSeenFrom(pre))
  }

  /** An overloaded denotation consisting of the alternatives of both given denotations.
   */
  case class MultiDenotation(denot1: Denotation, denot2: Denotation) extends Denotation(NoSymbol) with MultiPreDenotation {
    final def infoOrCompleter = multiHasNot("info")
    final def info(implicit ctx: Context) = infoOrCompleter
    final def validFor = denot1.validFor & denot2.validFor
    final def isType = false
    final def hasUniqueSym = false
    final def name(implicit ctx: Context) = denot1.name
    final def signature(implicit ctx: Context) = Signature.OverloadedSignature
    def atSignature(sig: Signature, site: Type, relaxed: Boolean)(implicit ctx: Context): Denotation =
      if (sig eq Signature.OverloadedSignature) this
      else derivedUnionDenotation(denot1.atSignature(sig, site, relaxed), denot2.atSignature(sig, site, relaxed))
    def current(implicit ctx: Context): Denotation =
      derivedUnionDenotation(denot1.current, denot2.current)
    def altsWith(p: Symbol => Boolean): List[SingleDenotation] =
      denot1.altsWith(p) ++ denot2.altsWith(p)
    def suchThat(p: Symbol => Boolean)(implicit ctx: Context): SingleDenotation = {
      val sd1 = denot1.suchThat(p)
      val sd2 = denot2.suchThat(p)
      if (sd1.exists)
        if (sd2.exists)
          if (isDoubleDef(denot1.symbol, denot2.symbol)) doubleDefError(denot1, denot2)
          else throw new TypeError(i"failure to disambiguate overloaded reference at $this")
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
      else derivedUnionDenotation(d1, d2)
    }
    def mapInfo(f: Type => Type)(implicit ctx: Context): Denotation =
      derivedUnionDenotation(denot1.mapInfo(f), denot2.mapInfo(f))
    def derivedUnionDenotation(d1: Denotation, d2: Denotation): Denotation =
      if ((d1 eq denot1) && (d2 eq denot2)) this
      else if (!d1.exists) d2
      else if (!d2.exists) d1
      else MultiDenotation(d1, d2)
    type AsSeenFromResult = Denotation
    def computeAsSeenFrom(pre: Type)(implicit ctx: Context): Denotation =
      derivedUnionDenotation(denot1.asSeenFrom(pre), denot2.asSeenFrom(pre))
    override def toString = alternatives.mkString(" <and> ")

    private def multiHasNot(op: String): Nothing =
      throw new UnsupportedOperationException(
        s"multi-denotation with alternatives $alternatives does not implement operation $op")
  }

  // --------------- Context Base Trait -------------------------------

  trait DenotationsBase { this: ContextBase =>

    /** The current denotation of the static reference given by path,
     *  or a MissingRef or NoQualifyingRef instance, if it does not exist.
     *  if generateStubs is set, generates stubs for missing top-level symbols
     */
    def staticRef(path: Name, generateStubs: Boolean = true, isPackage: Boolean = false)(implicit ctx: Context): Denotation = {
      def select(prefix: Denotation, selector: Name): Denotation = {
        val owner = prefix.disambiguate(_.info.isParameterless)
        def isPackageFromCoreLibMissing: Boolean = {
          owner.symbol == defn.RootClass &&
          (
            selector == nme.scala_ || // if the scala package is missing, the stdlib must be missing
            selector == nme.scalaShadowing // if the scalaShadowing package is missing, the dotty library must be missing
          )
        }
        if (owner.exists) {
          val result = if (isPackage) owner.info.decl(selector) else owner.info.member(selector)
          if (result.exists) result
          else {
            val alt =
              if (generateStubs) missingHook(owner.symbol.moduleClass, selector)
              else NoSymbol
            if (alt.exists) alt.denot
            else if (isPackageFromCoreLibMissing) throw new MissingCoreLibraryException(selector.toString)
            else MissingRef(owner, selector)
          }
        }
        else owner
      }
      def recur(path: Name, wrap: TermName => Name = identity): Denotation = path match {
        case path: TypeName =>
          recur(path.toTermName, n => n.toTypeName)
        case ModuleClassName(underlying) =>
          recur(underlying, n => wrap(ModuleClassName(n)))
        case QualifiedName(prefix, selector) =>
          select(recur(prefix), wrap(selector))
        case qn @ AnyQualifiedName(prefix, _) =>
          recur(prefix, n => wrap(qn.info.mkString(n).toTermName))
        case path: SimpleName =>
          def recurSimple(len: Int, wrap: TermName => Name): Denotation = {
            val point = path.lastIndexOf('.', len - 1)
            val selector = wrap(path.slice(point + 1, len).asTermName)
            val prefix =
              if (point > 0) recurSimple(point, identity)
              else if (selector.isTermName) defn.RootClass.denot
              else defn.EmptyPackageClass.denot
            select(prefix, selector)
          }
          recurSimple(path.length, wrap)
      }
      recur(path)
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

  /** An exception for accessing symbols that are no longer valid in current run */
  class StaleSymbol(msg: => String) extends Exception {
    util.Stats.record("stale symbol")
    override def getMessage() = msg
  }
}
