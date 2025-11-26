package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Flags.*, Names.*, NameOps.*, Denotations.*
import Decorators.*
import Phases.{gettersPhase, elimByNamePhase}
import StdNames.nme
import TypeOps.refineUsingParent
import collection.mutable
import util.{Stats, NoSourcePosition, EqHashMap}
import config.Config
import config.Feature.{migrateTo3, sourceVersion}
import config.Printers.{subtyping, gadts, matchTypes, capt, noPrinter}
import config.SourceVersion
import TypeErasure.{erasedLub, erasedGlb}
import TypeApplications.*
import Variances.{Variance, variancesConform}
import Constants.Constant
import scala.util.control.NonFatal
import typer.ProtoTypes.constrained
import typer.Applications.productSelectorTypes
import reporting.trace
import annotation.constructorOnly
import cc.*
import Capabilities.Capability
import NameKinds.WildcardParamName
import MatchTypes.isConcrete
import reporting.Message.Note
import scala.util.boundary, boundary.break

object MatchReducer:
  import printing.*, Texts.*
  enum MatchResult extends Showable:
    case Reduced(tp: Type)
    case Disjoint
    case ReducedAndDisjoint
    case Stuck
    case NoInstance(fails: List[(Name, TypeBounds)])

    def toText(p: Printer): Text = this match
      case Reduced(tp)        => "Reduced(" ~ p.toText(tp) ~ ")"
      case Disjoint           => "Disjoint"
      case ReducedAndDisjoint => "ReducedAndDisjoint"
      case Stuck              => "Stuck"
      case NoInstance(fails)  => "NoInstance(" ~ Text(fails.map(p.toText(_) ~ p.toText(_)), ", ") ~ ")"

/** A type comparer for reducing match types.
 *  TODO: Not sure this needs to be a type comparer. Can we make it a
 *  separate class?
 */
class MatchReducer(initctx: Context) extends TypeComparer(initctx) {
  import MatchReducer.*

  init(initctx)

  override def matchReducer = this

  def matchCases(scrut: Type, cases: List[MatchTypeCaseSpec])(using Context): Type = {
    // a reference for the type parameters poisoned during matching
    // for use during the reduction step
    var poisoned: Set[TypeParamRef] = Set.empty

    def paramInstances(canApprox: Boolean) = new TypeAccumulator[Array[Type]]:
      def apply(insts: Array[Type], t: Type) = t match
        case param @ TypeParamRef(b, n) if b eq caseLambda =>
          insts(n) =
            if canApprox then
              approximation(param, fromBelow = variance >= 0, Int.MaxValue).simplified
            else constraint.entry(param) match
              case entry: TypeBounds =>
                val lo = fullLowerBound(param)
                val hi = fullUpperBound(param)
                if !poisoned(param) && isSubType(hi, lo) then lo.simplified else Range(lo, hi)
              case inst =>
                assert(inst.exists, i"param = $param\nconstraint = $constraint")
                if !poisoned(param) then inst.simplified else Range(inst, inst)
          insts
        case _ =>
          foldOver(insts, t)

    def instantiateParams(insts: Array[Type]) = new ApproximatingTypeMap {
      variance = 0

      override def range(lo: Type, hi: Type): Type =
        if variance == 0 && (lo eq hi) then
          // override the default `lo eq hi` test, which removes the Range
          // which leads to a Reduced result, instead of NoInstance
          Range(lower(lo), upper(hi))
        else super.range(lo, hi)

      def apply(t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda => insts(n)
        case t: LazyRef => apply(t.ref)
        case _ => mapOver(t)
      }
    }

    def instantiateParamsSpec(insts: Array[Type], caseLambda: HKTypeLambda) = new TypeMap {
      variance = 0

      def apply(t: Type) = t match {
        case t @ TypeParamRef(b, n) if b `eq` caseLambda => insts(n)
        case t: LazyRef => apply(t.ref)
        case _ => mapOver(t)
      }
    }

    /** Match a single case. */
    def matchCase(cas: MatchTypeCaseSpec): MatchResult = trace(i"$scrut match ${MatchTypeTrace.caseText(cas)}", matchTypes, show = true) {
      cas match
        case cas: MatchTypeCaseSpec.SubTypeTest     => matchSubTypeTest(cas)
        case cas: MatchTypeCaseSpec.SpeccedPatMat   => matchSpeccedPatMat(cas)
        case cas: MatchTypeCaseSpec.LegacyPatMat    => matchLegacyPatMat(cas)
        case cas: MatchTypeCaseSpec.MissingCaptures => matchMissingCaptures(cas)
    }

    def matchSubTypeTest(spec: MatchTypeCaseSpec.SubTypeTest): MatchResult =
      val disjoint = provablyDisjoint(scrut, spec.pattern)
      if necessarySubType(scrut, spec.pattern) then
        if disjoint then
          MatchResult.ReducedAndDisjoint
        else
          MatchResult.Reduced(spec.body)
      else if disjoint then
        MatchResult.Disjoint
      else
        MatchResult.Stuck
    end matchSubTypeTest

    // See https://docs.scala-lang.org/sips/match-types-spec.html#matching
    def matchSpeccedPatMat(spec: MatchTypeCaseSpec.SpeccedPatMat): MatchResult =
      val instances = Array.fill[Type](spec.captureCount)(NoType)
      val noInstances = mutable.ListBuffer.empty[(TypeName, TypeBounds)]

      def rec(pattern: MatchTypeCasePattern, scrut: Type, variance: Int, scrutIsWidenedAbstract: Boolean): Boolean =
        pattern match
          case MatchTypeCasePattern.Capture(num, /* isWildcard = */ true) =>
            // instantiate the wildcard in a way that the subtype test always succeeds
            instances(num) = variance match
              case 1  => scrut.hiBound // actually important if we are not in a class type constructor
              case -1 => scrut.loBound
              case 0  => scrut
            !instances(num).isError

          case MatchTypeCasePattern.Capture(num, /* isWildcard = */ false) =>
            def failNotSpecific(bounds: TypeBounds): TypeBounds =
              noInstances += spec.origMatchCase.paramNames(num) -> bounds
              bounds

            instances(num) = scrut match
              case scrut: TypeBounds =>
                if scrutIsWidenedAbstract then
                  failNotSpecific(scrut)
                else
                  variance match
                    case 1  => scrut.hi
                    case -1 => scrut.lo
                    case 0  => failNotSpecific(scrut)
              case _ =>
                if scrutIsWidenedAbstract && variance != 0 then
                  // fail as not specific
                  // the Nothing and Any bounds are used so that they are not displayed; not for themselves in particular
                  if variance > 0 then failNotSpecific(TypeBounds(defn.NothingType, scrut))
                  else failNotSpecific(TypeBounds(scrut, defn.AnyType))
                else
                  scrut
            !instances(num).isError

          case MatchTypeCasePattern.TypeTest(tpe) =>
            // The actual type test is handled by `scrut <:< instantiatedPat`
            true

          case MatchTypeCasePattern.BaseTypeTest(classType, argPatterns, needsConcreteScrut) =>
            val cls = classType.classSymbol.asClass
            scrut.baseType(cls) match
              case base @ AppliedType(baseTycon, baseArgs) =>
                // #19445 Don't check the prefix of baseTycon here; it is handled by `scrut <:< instantiatedPat`.
                val innerScrutIsWidenedAbstract =
                  scrutIsWidenedAbstract
                    || (needsConcreteScrut && !isConcrete(scrut)) // no point in checking concreteness if it does not need to be concrete
                matchArgs(argPatterns, baseArgs, classType.typeParams, innerScrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.AbstractTypeConstructor(tycon, argPatterns) =>
            scrut.dealias match
              case scrutDealias @ AppliedType(scrutTycon, args) if scrutTycon =:= tycon =>
                matchArgs(argPatterns, args, tycon.typeParams, scrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.CompileTimeS(argPattern) =>
            natValue(scrut) match
              case Some(scrutValue) if scrutValue > 0 =>
                rec(argPattern, ConstantType(Constant(scrutValue - 1)), variance, scrutIsWidenedAbstract)
              case _ =>
                false

          case MatchTypeCasePattern.TypeMemberExtractor(typeMemberName, capture) =>
            /** Try to remove references to `skolem` from a type in accordance with the spec.
             *
             *  References to `skolem` occuring are avoided by following aliases and
             *  singletons.
             *
             *  If any reference to `skolem` remains in the result type,
             *  `refersToSkolem` is set to true.
             */
            class DropSkolemMap(skolem: SkolemType) extends TypeMap:
              var refersToSkolem = false
              def apply(tp: Type): Type =
                if refersToSkolem then
                  return tp
                tp match
                  case `skolem` =>
                    refersToSkolem = true
                    tp
                  case tp: NamedType =>
                    val pre1 = apply(tp.prefix)
                    if refersToSkolem then
                      tp match
                        case tp: TermRef => tp.info.widenExpr.dealias match
                          case info: SingletonType =>
                            refersToSkolem = false
                            apply(info)
                          case _ =>
                            tp.derivedSelect(pre1)
                        case tp: TypeRef => tp.info match
                          case info: AliasingBounds =>
                            refersToSkolem = false
                            apply(info.alias)
                          case _ =>
                            tp.derivedSelect(pre1)
                    else
                      tp.derivedSelect(pre1)
                  case tp: LazyRef =>
                    // By default, TypeMap maps LazyRefs lazily. We need to
                    // force it for `refersToSkolem` to be correctly set.
                    apply(tp.ref)
                  case _ =>
                    mapOver(tp)
            end DropSkolemMap
            /** Try to remove references to `skolem` from `u` in accordance with the spec.
             *
             *  If any reference to `skolem` remains in the result type, return
             *  NoType instead.
             */
            def dropSkolem(u: Type, skolem: SkolemType): Type =
              val dmap = DropSkolemMap(skolem)
              val res = dmap(u)
              if dmap.refersToSkolem then NoType else res

            val stableScrut: SingletonType = scrut match
              case scrut: SingletonType => scrut
              case _                    => SkolemType(scrut)

            stableScrut.member(typeMemberName) match
              case denot: SingleDenotation if denot.exists =>
                val info = stableScrut match
                  case skolem: SkolemType =>
                    /* If it is a skolem type, we cannot have class selections nor
                     * abstract type selections. If it is an alias, we try to remove
                     * any reference to the skolem from the right-hand-side. If that
                     * succeeds, we take the result, otherwise we fail as not-specific.
                     */

                    def adaptToTriggerNotSpecific(info: Type): Type = info match
                      case info: TypeBounds => info
                      case _                => RealTypeBounds(info, info)

                    denot.info match
                      case denotInfo: AliasingBounds =>
                        val alias = denotInfo.alias
                        dropSkolem(alias, skolem).orElse(adaptToTriggerNotSpecific(alias))
                      case ClassInfo(prefix, cls, _, _, _) =>
                        // for clean error messages
                        adaptToTriggerNotSpecific(prefix.select(cls))
                      case denotInfo =>
                        adaptToTriggerNotSpecific(denotInfo)

                  case _ =>
                    // The scrutinee type is truly stable. We select the type member directly on it.
                    stableScrut.select(typeMemberName)
                end info

                rec(capture, info, variance = 0, scrutIsWidenedAbstract)

              case _ =>
                // The type member was not found; no match
                false
      end rec

      def matchArgs(argPatterns: List[MatchTypeCasePattern], args: List[Type], tparams: List[TypeParamInfo], scrutIsWidenedAbstract: Boolean): Boolean =
        if argPatterns.isEmpty then
          true
        else
          rec(argPatterns.head, args.head, tparams.head.paramVarianceSign, scrutIsWidenedAbstract)
            && matchArgs(argPatterns.tail, args.tail, tparams.tail, scrutIsWidenedAbstract)

      // This might not be needed
      val constrainedCaseLambda = constrained(spec.origMatchCase, ast.tpd.EmptyTree)._1.asInstanceOf[HKTypeLambda]

      val disjoint =
        val defn.MatchCase(origPattern, _) = constrainedCaseLambda.resultType: @unchecked
        provablyDisjoint(scrut, origPattern)

      def tryDisjoint: MatchResult =
        if disjoint then
          MatchResult.Disjoint
        else
          MatchResult.Stuck

      if rec(spec.pattern, scrut, variance = 1, scrutIsWidenedAbstract = false) then
        if noInstances.nonEmpty then
          MatchResult.NoInstance(noInstances.toList)
        else
          val defn.MatchCase(instantiatedPat, reduced) =
            instantiateParamsSpec(instances, constrainedCaseLambda)(constrainedCaseLambda.resultType): @unchecked
          if scrut <:< instantiatedPat then
            if disjoint then
              MatchResult.ReducedAndDisjoint
            else
              MatchResult.Reduced(reduced)
          else
            tryDisjoint
      else
        tryDisjoint
    end matchSpeccedPatMat

    def matchLegacyPatMat(spec: MatchTypeCaseSpec.LegacyPatMat): MatchResult =
      val caseLambda = constrained(spec.origMatchCase, ast.tpd.EmptyTree)._1.asInstanceOf[HKTypeLambda]
      this.caseLambda = caseLambda

      val defn.MatchCase(pat, body) = caseLambda.resultType: @unchecked

      def matches(canWidenAbstract: Boolean): Boolean =
        val saved = this.canWidenAbstract
        val savedPoisoned = this.poisoned
        this.canWidenAbstract = canWidenAbstract
        this.poisoned = Set.empty
        try necessarySubType(scrut, pat)
        finally
          poisoned = this.poisoned
          this.poisoned = savedPoisoned
          this.canWidenAbstract = saved

      val disjoint = provablyDisjoint(scrut, pat)

      def redux(canApprox: Boolean): MatchResult =
        val instances = paramInstances(canApprox)(Array.fill(caseLambda.paramNames.length)(NoType), pat)
        instantiateParams(instances)(body) match
          case Range(lo, hi) =>
            MatchResult.NoInstance {
              caseLambda.paramNames.zip(instances).collect {
                case (name, Range(lo, hi)) => (name, TypeBounds(lo, hi))
              }
            }
          case redux =>
            if disjoint then
              MatchResult.ReducedAndDisjoint
            else
              MatchResult.Reduced(redux)

      if matches(canWidenAbstract = false) then
        redux(canApprox = true)
      else if matches(canWidenAbstract = true) then
        redux(canApprox = false)
      else if (disjoint)
        // We found a proof that `scrut` and `pat` are incompatible.
        // The search continues.
        MatchResult.Disjoint
      else
        MatchResult.Stuck
    end matchLegacyPatMat

    def matchMissingCaptures(spec: MatchTypeCaseSpec.MissingCaptures): MatchResult =
      MatchResult.Stuck

    def recur(remaining: List[MatchTypeCaseSpec]): Type = remaining match
      case (cas: MatchTypeCaseSpec.LegacyPatMat) :: _ if sourceVersion.isAtLeast(SourceVersion.`3.4`) =>
        val errorText = MatchTypeTrace.illegalPatternText(scrut, cas)
        ErrorType(reporting.MatchTypeLegacyPattern(errorText))
      case cas :: remaining1 =>
        matchCase(cas) match
          case MatchResult.Disjoint =>
            recur(remaining1)
          case MatchResult.Stuck =>
            MatchTypeTrace.stuck(scrut, cas, remaining1)
            NoType
          case MatchResult.NoInstance(fails) =>
            MatchTypeTrace.noInstance(scrut, cas, fails)
            NoType
          case MatchResult.Reduced(tp) =>
            tp.simplified
          case MatchResult.ReducedAndDisjoint =>
            // Empty types break the basic assumption that if a scrutinee and a
            // pattern are disjoint it's OK to reduce passed that pattern. Indeed,
            // empty types viewed as a set of value is always a subset of any other
            // types. As a result, if a scrutinee both matches a pattern and is
            // probably disjoint from it, we prevent reduction.
            // See `tests/neg/6570.scala` and `6570-1.scala` for examples that
            // exploit emptiness to break match type soundness.
            MatchTypeTrace.emptyScrutinee(scrut)
            NoType
      case Nil =>
        /* TODO warn ? then re-enable warn/12974.scala:26
        val noCasesText = MatchTypeTrace.noMatchesText(scrut, cases)
        report.warning(reporting.MatchTypeNoCases(noCasesText), pos = ???)
        */
        MatchTypeTrace.noMatches(scrut, cases)
        NoType

    inFrozenConstraint(recur(cases))
  }
}