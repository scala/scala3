package dotty.tools
package dotc
package core

import Contexts._, Types._, Symbols._, Names._, Flags._
import SymDenotations._
import util.Spans._
import util.Stats
import util.SourcePosition
import NameKinds.DepParamName
import Decorators._
import StdNames._
import collection.mutable
import ast.tpd._
import reporting.trace
import reporting.diagnostic.Message
import config.Printers.{gadts, typr}
import typer.Applications._
import typer.ProtoTypes._
import typer.ForceDegree
import typer.Inferencing.isFullyDefined

import scala.annotation.internal.sharable

trait TypeOps { this: Context => // TODO: Make standalone object.

  /** The type `tp` as seen from prefix `pre` and owner `cls`. See the spec
   *  for what this means.
   */
  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol): Type = {
    pre match {
      case pre: QualSkolemType =>
        // When a selection has an unstable qualifier, the qualifier type gets
        // wrapped in a `QualSkolemType` so that it may appear soundly as the
        // prefix of a path in the selection type.
        // However, we'd like to avoid referring to skolems when possible since
        // they're an extra level of indirection we usually don't need, so we
        // compute the type as seen from the widened prefix, and in the rare
        // cases where this leads to an approximated type we recompute it with
        // the skolemized prefix. See the i6199* tests for usecases.
        val widenedAsf = new AsSeenFromMap(pre.info, cls)
        val ret = widenedAsf.apply(tp)

        if (!widenedAsf.approximated)
          return ret

        Stats.record("asSeenFrom skolem prefix required")
      case _ =>
    }

    new AsSeenFromMap(pre, cls).apply(tp)
  }

  /** The TypeMap handling the asSeenFrom */
  class AsSeenFromMap(pre: Type, cls: Symbol) extends ApproximatingTypeMap {
    /** Set to true when the result of `apply` was approximated to avoid an unstable prefix. */
    var approximated: Boolean = false

    def apply(tp: Type): Type = {

      /** Map a `C.this` type to the right prefix. If the prefix is unstable, and
       *  the current variance is <= 0, return a range.
       */
      def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type = /*>|>*/ trace.conditionally(TypeOps.track, s"toPrefix($pre, $cls, $thiscls)", show = true) /*<|<*/ {
        if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
          tp
        else pre match {
          case pre: SuperType => toPrefix(pre.thistpe, cls, thiscls)
          case _ =>
            if (thiscls.derivesFrom(cls) && pre.baseType(thiscls).exists)
              if (variance <= 0 && !isLegalPrefix(pre)) {
                if (variance < 0) {
                  approximated = true
                  defn.NothingType
                }
                else
                  // Don't set the `approximated` flag yet: if this is a prefix
                  // of a path, we might be able to dealias the path instead
                  // (this is handled in `ApproximatingTypeMap`). If dealiasing
                  // is not possible, then `expandBounds` will end up being
                  // called which we override to set the `approximated` flag.
                  range(defn.NothingType, pre)
              }
              else pre
            else if ((pre.termSymbol is Package) && !(thiscls is Package))
              toPrefix(pre.select(nme.PACKAGE), cls, thiscls)
            else
              toPrefix(pre.baseType(cls).normalizedPrefix, cls.owner, thiscls)
        }
      }

      /*>|>*/ trace.conditionally(TypeOps.track, s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) /*<|<*/ { // !!! DEBUG
        // All cases except for ThisType are the same as in Map. Inlined for performance
        // TODO: generalize the inlining trick?
        tp match {
          case tp: NamedType =>
            val sym = tp.symbol
            if (sym.isStatic && !sym.maybeOwner.isOpaqueCompanion || (tp.prefix `eq` NoPrefix)) tp
            else derivedSelect(tp, atVariance(variance max 0)(this(tp.prefix)))
          case tp: ThisType =>
            toPrefix(pre, cls, tp.cls)
          case _: BoundType =>
            tp
          case _ =>
            mapOver(tp)
        }
      }
    }

    override def reapply(tp: Type): Type =
      // derived infos have already been subjected to asSeenFrom, hence to need to apply the map again.
      tp

    override protected def expandBounds(tp: TypeBounds): Type = {
      approximated = true
      super.expandBounds(tp)
    }
  }

  def isLegalPrefix(pre: Type)(implicit ctx: Context): Boolean =
    pre.isStable || !ctx.phase.isTyper

  /** Implementation of Types#simplified */
  final def simplify(tp: Type, theMap: SimplifyMap): Type = {
    def mapOver = (if (theMap != null) theMap else new SimplifyMap).mapOver(tp)
    tp match {
      case tp: NamedType =>
        if (tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) tp
        else tp.derivedSelect(simplify(tp.prefix, theMap)) match {
          case tp1: NamedType if tp1.denotationIsCurrent =>
            val tp2 = tp1.reduceProjection
            //if (tp2 ne tp1) println(i"simplified $tp1 -> $tp2")
            tp2
          case tp1 => tp1
        }
      case tp: TypeParamRef =>
        if (tp.paramName.is(DepParamName)) {
          val bounds = ctx.typeComparer.bounds(tp)
          if (bounds.lo.isRef(defn.NothingClass)) bounds.hi else bounds.lo
        }
        else {
          val tvar = typerState.constraint.typeVarOfParam(tp)
          if (tvar.exists) tvar else tp
        }
      case  _: ThisType | _: BoundType =>
        tp
      case tp: AliasingBounds =>
        tp.derivedAlias(simplify(tp.alias, theMap))
      case AndType(l, r) if !ctx.mode.is(Mode.Type) =>
        simplify(l, theMap) & simplify(r, theMap)
      case OrType(l, r) if !ctx.mode.is(Mode.Type) =>
        simplify(l, theMap) | simplify(r, theMap)
      case _: AppliedType | _: MatchType =>
        val normed = tp.tryNormalize
        if (normed.exists) normed else mapOver
      case _ =>
        mapOver
    }
  }

  class SimplifyMap extends TypeMap {
    def apply(tp: Type): Type = simplify(tp, this)
  }

  /** Approximate union type by intersection of its dominators.
   *  That is, replace a union type Tn | ... | Tn
   *  by the smallest intersection type of base-class instances of T1,...,Tn.
   *  Example: Given
   *
   *      trait C[+T]
   *      trait D
   *      class A extends C[A] with D
   *      class B extends C[B] with D with E
   *
   *  we approximate `A | B` by `C[A | B] with D`.
   *
   *  Before we do that, we try to find a common non-class supertype of T1 | ... | Tn
   *  in a "best effort", ad-hoc way by selectively widening types in `T1, ..., Tn`
   *  and stopping if the resulting union simplifies to a type that is not a disjunction.
   */
  def orDominator(tp: Type): Type = {

    /** a faster version of cs1 intersect cs2 */
    def intersect(cs1: List[ClassSymbol], cs2: List[ClassSymbol]): List[ClassSymbol] = {
      val cs2AsSet = new util.HashSet[ClassSymbol](128)
      cs2.foreach(cs2AsSet.addEntry)
      cs1.filter(cs2AsSet.contains)
    }

    /** The minimal set of classes in `cs` which derive all other classes in `cs` */
    def dominators(cs: List[ClassSymbol], accu: List[ClassSymbol]): List[ClassSymbol] = (cs: @unchecked) match {
      case c :: rest =>
        val accu1 = if (accu exists (_ derivesFrom c)) accu else c :: accu
        if (cs == c.baseClasses) accu1 else dominators(rest, accu1)
      case Nil => // this case can happen because after erasure we do not have a top class anymore
        assert(ctx.erasedTypes || ctx.reporter.errorsReported)
        defn.ObjectClass :: Nil
    }

    def mergeRefinedOrApplied(tp1: Type, tp2: Type): Type = {
      def fail = throw new AssertionError(i"Failure to join alternatives $tp1 and $tp2")
      tp1 match {
        case tp1 @ RefinedType(parent1, name1, rinfo1) =>
          tp2 match {
            case RefinedType(parent2, `name1`, rinfo2) =>
              tp1.derivedRefinedType(
                mergeRefinedOrApplied(parent1, parent2), name1, rinfo1 | rinfo2)
            case _ => fail
          }
        case tp1 @ AppliedType(tycon1, args1) =>
          tp2 match {
            case AppliedType(tycon2, args2) =>
              tp1.derivedAppliedType(
                mergeRefinedOrApplied(tycon1, tycon2),
                ctx.typeComparer.lubArgs(args1, args2, tycon1.typeParams))
            case _ => fail
          }
        case tp1 @ TypeRef(pre1, _) =>
          tp2 match {
            case tp2 @ TypeRef(pre2, _) if tp1.name eq tp2.name =>
              tp1.derivedSelect(pre1 | pre2)
            case _ => fail
          }
        case _ => fail
      }
    }

    def approximateOr(tp1: Type, tp2: Type): Type = {
      def isClassRef(tp: Type): Boolean = tp match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: AppliedType => isClassRef(tp.tycon)
        case tp: RefinedType => isClassRef(tp.parent)
        case _ => false
      }

      // Step 1: Get RecTypes and ErrorTypes out of the way,
      tp1 match {
        case tp1: RecType => return tp1.rebind(approximateOr(tp1.parent, tp2))
        case err: ErrorType => return err
        case _ =>
      }
      tp2 match {
        case tp2: RecType => return tp2.rebind(approximateOr(tp1, tp2.parent))
        case err: ErrorType => return err
        case _ =>
      }

      // Step 2: Try to widen either side. This is tricky and incomplete.
      // An illustration is in test pos/padTo.scala: Here we need to compute the join of
      //
      //   `A | C` under the constraints `B >: A` and `C <: B`
      //
      // where `A, B, C` are type parameters.
      // Widening `A` to its upper bound would give `Any | C`, i.e. `Any`.
      // But widening `C` first would give `A | B` and then `B`.
      // So we need to widen `C` first. But how to decide this in general?
      // In the algorithm below, we try to widen both sides (once), and then proceed as follows:
      //
      //  2.0. If no widening succeeds, proceed with step 3.
      //  2.1. If only one widening succeeds, continue with that one.
      //  2.2. If the two widened types are in a subtype relationship, continue with the smaller one.
      //  2.3. If exactly one of the two types is a singleton type, continue with the widened singleton type.
      //  2.4. If the widened tp2 is a supertype of tp1, return widened tp2.
      //  2.5. If the widened tp1 is a supertype of tp2, return widened tp1.
      //  2.6. Otherwise, continue with widened tp1
      //
      // At steps 4-6 we lose possible solutions, since we have to make an
      // arbitrary choice which side to widen. A better solution would look at
      // the constituents of each operand (if the operand is an OrType again) and
      // try to widen them selectively in turn. But this might lead to a combinatorial
      // explosion of possibilities.
      //
      // Another approach could be to store information contained in lower bounds
      // on both sides. So if `B >: A` we'd also record that `A <: B` and therefore
      // widening `A` would yield `B` instead of `Any`, so we'd still be on the right track.
      // This looks feasible if lower bounds are type parameters, but tricky if they
      // are something else. We'd have to extract the strongest possible
      // constraint over all type parameters that is implied by a lower bound.
      // This looks related to an algorithmic problem arising in GADT matching.
      //
      // However, this alone is still not enough. There are other sources of incompleteness,
      // for instance arising from mis-aligned refinements.
      val tp1w = tp1 match {
        case tp1: TypeProxy if !isClassRef(tp1) => tp1.superType.widenExpr
        case _ => tp1
      }
      val tp2w = tp2 match {
        case tp2: TypeProxy if !isClassRef(tp2) => tp2.superType.widenExpr
        case _ => tp2
      }
      if ((tp1w ne tp1) || (tp2w ne tp2)) {
        val isSingle1 = tp1.isInstanceOf[SingletonType]
        val isSingle2 = tp2.isInstanceOf[SingletonType]
        return {
          if (tp2w eq tp2) orDominator(tp1w | tp2)                  // 2.1
          else if (tp1w eq tp1) orDominator(tp1 | tp2w)             // 2.1
          else if (tp1w frozen_<:< tp2w) orDominator(tp1w | tp2)    // 2.2
          else if (tp2w frozen_<:< tp1w) orDominator(tp1 | tp2w)    // 2.2
          else if (isSingle1 && !isSingle2) orDominator(tp1w | tp2) // 2.3
          else if (isSingle2 && !isSingle1) orDominator(tp1 | tp2w) // 2.3
          else if (tp1 frozen_<:< tp2w) tp2w                        // 2.4
          else if (tp2 frozen_<:< tp1w) tp1w                        // 2.5
          else orDominator(tp1w | tp2)                              // 2.6
        }
      }

      // Step 3: Intersect base classes of both sides
      val commonBaseClasses = tp.mapReduceOr(_.baseClasses)(intersect)
      val doms = dominators(commonBaseClasses, Nil)
      def baseTp(cls: ClassSymbol): Type =
        tp.baseType(cls).mapReduceOr(identity)(mergeRefinedOrApplied)
      doms.map(baseTp).reduceLeft(AndType.apply)
    }

    tp match {
      case tp: OrType =>
        approximateOr(tp.tp1, tp.tp2)
      case _ =>
        tp
    }
  }

  /** If `tpe` is of the form `p.x` where `p` refers to a package
   *  but `x` is not owned by a package, expand it to
   *
   *      p.package.x
   */
  def makePackageObjPrefixExplicit(tpe: NamedType): Type = {
    def tryInsert(pkgClass: SymDenotation): Type = pkgClass match {
      case pkg: PackageClassDenotation =>
        val pobj = pkg.packageObjFor(tpe.symbol)
        if (pobj.exists) tpe.derivedSelect(pobj.termRef)
        else tpe
      case _ =>
        tpe
    }
    if (tpe.symbol.isRoot)
      tpe
    else
      tpe.prefix match {
        case pre: ThisType if pre.cls is Package => tryInsert(pre.cls)
        case pre: TermRef if pre.symbol is Package => tryInsert(pre.symbol.moduleClass)
        case _ => tpe
      }
  }

  /** An argument bounds violation is a triple consisting of
   *   - the argument tree
   *   - a string "upper" or "lower" indicating which bound is violated
   *   - the violated bound
   */
  type BoundsViolation = (Tree, String, Type)

  /** The list of violations where arguments are not within bounds.
   *  @param  args          The arguments
   *  @param  boundss       The list of type bounds
   *  @param  instantiate   A function that maps a bound type and the list of argument types to a resulting type.
   *                        Needed to handle bounds that refer to other bounds.
   *  @param  app           The applied type whose arguments are checked, or NoType if
   *                        arguments are for a TypeApply.
   *
   *  This is particularly difficult for F-bounds that also contain wildcard arguments (see below).
   *  In fact the current treatment for this sitiuation can so far only be classified as "not obviously wrong",
   *  (maybe it still needs to be revised).
   */
  def boundsViolations(args: List[Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type, app: Type)(implicit ctx: Context): List[BoundsViolation] = {
    val argTypes = args.tpes

    /** Replace all wildcards in `tps` with `<app>#<tparam>` where `<tparam>` is the
     *  type parameter corresponding to the wildcard.
     */
    def skolemizeWildcardArgs(tps: List[Type], app: Type) = app match {
      case AppliedType(tycon, args) if tycon.typeSymbol.isClass && !scala2Mode =>
        tps.zipWithConserve(tycon.typeSymbol.typeParams) {
          (tp, tparam) => tp match {
            case _: TypeBounds => app.select(tparam)
            case _ => tp
          }
        }
      case _ => tps
    }

    // Skolemized argument types are used to substitute in F-bounds.
    val skolemizedArgTypes = skolemizeWildcardArgs(argTypes, app)
    val violations = new mutable.ListBuffer[BoundsViolation]

    for ((arg, bounds) <- args zip boundss) {
      def checkOverlapsBounds(lo: Type, hi: Type): Unit = {
        //println(i" = ${instantiate(bounds.hi, argTypes)}")

        var checkCtx = ctx  // the context to be used for bounds checking
        if (argTypes ne skolemizedArgTypes) { // some of the arguments are wildcards

          /** Is there a `LazyRef(TypeRef(_, sym))` reference in `tp`? */
          def isLazyIn(sym: Symbol, tp: Type): Boolean = {
            def isReference(tp: Type) = tp match {
              case tp: LazyRef => tp.ref.isInstanceOf[TypeRef] && tp.ref.typeSymbol == sym
              case _ => false
            }
            tp.existsPart(isReference, forceLazy = false)
          }

          /** The argument types of the form `TypeRef(_, sym)` which appear as a LazyRef in `bounds`.
           *  This indicates that the application is used as an F-bound for the symbol referred to in the LazyRef.
           */
          val lazyRefs = skolemizedArgTypes collect {
            case tp: TypeRef if isLazyIn(tp.symbol, bounds) => tp.symbol
          }

          for (sym <- lazyRefs) {

            // If symbol `S` has an F-bound such as `C[_, S]` that contains wildcards,
            // add a modifieed bound where wildcards are skolemized as a GADT bound for `S`.
            // E.g. for `C[_, S]` we would add `C[C[_, S]#T0, S]` where `T0` is the first
            // type parameter of `C`. The new bound is added as a GADT bound for `S` in
            // `checkCtx`.
            // This mirrors what we do for the bounds that are checked and allows us thus
            // to bounds-check F-bounds with wildcards. A test case is pos/i6146.scala.

            def massage(tp: Type): Type = tp match {
              case tp @ AppliedType(tycon, args) =>
                tp.derivedAppliedType(tycon, skolemizeWildcardArgs(args, tp))
              case tp: AndOrType =>
                tp.derivedAndOrType(massage(tp.tp1), massage(tp.tp2))
              case _ => tp
            }
            def narrowBound(bound: Type, fromBelow: Boolean): Unit = {
              val bound1 = massage(bound)
              if (bound1 ne bound) {
                if (checkCtx eq ctx) checkCtx = ctx.fresh.setFreshGADTBounds
                if (!checkCtx.gadt.contains(sym)) checkCtx.gadt.addEmptyBounds(sym)
                checkCtx.gadt.addBound(sym, bound1, fromBelow)
                typr.println("install GADT bound $bound1 for when checking F-bounded $sym")
              }
            }
            narrowBound(sym.info.loBound, fromBelow = true)
            narrowBound(sym.info.hiBound, fromBelow = false)
          }
        }

        val hiBound = instantiate(bounds.hi, skolemizedArgTypes)
        val loBound = instantiate(bounds.lo, skolemizedArgTypes)

        def check(implicit ctx: Context) = {
          if (!(lo <:< hiBound)) violations += ((arg, "upper", hiBound))
          if (!(loBound <:< hi)) violations += ((arg, "lower", loBound))
        }
        check(checkCtx)
      }
      arg.tpe match {
        case TypeBounds(lo, hi) => checkOverlapsBounds(lo, hi)
        case tp => checkOverlapsBounds(tp, tp)
      }
    }
    violations.toList
  }

  /** Are we in an inline method body? */
  def inInlineMethod: Boolean = owner.ownersIterator.exists(_.isInlineMethod)

  /** Is `feature` enabled in class `owner`?
   *  This is the case if one of the following two alternatives holds:
   *
   *  1. The feature is imported by a named import
   *
   *       import owner.feature
   *
   *     and there is no visible nested import that excludes the feature, as in
   *
   *       import owner.{ feature => _ }
   *
   *  The feature may be bunched with others, or renamed, but wildcard imports don't count.
   *
   *  2. The feature is enabled by a compiler option
   *
   *       - language:<prefix>feature
   *
   *  where <prefix> is the full name of the owner followed by a "." minus
   *  the prefix "dotty.language.".
   */
  def featureEnabled(feature: TermName, owner: Symbol = NoSymbol): Boolean = {
    def hasImport = {
      val owner1 = if (!owner.exists) defn.LanguageModuleClass else owner
      ctx.importInfo != null &&
      ctx.importInfo.featureImported(feature, owner1)(ctx.withPhase(ctx.typerPhase))
    }
    val hasOption = {
      def toPrefix(sym: Symbol): String =
        if (!sym.exists) ""
        else toPrefix(sym.owner) + sym.name + "."
      val featureName = toPrefix(owner) + feature
      ctx.base.settings.language.value exists (s => s == featureName || s == "_")
    }
    hasOption || hasImport
  }

  /** Is auto-tupling enabled? */
  def canAutoTuple: Boolean =
    !featureEnabled(nme.noAutoTupling)

  def scala2Mode: Boolean =
    featureEnabled(nme.Scala2)

  def dynamicsEnabled: Boolean =
    featureEnabled(nme.dynamics)

  def testScala2Mode(msg: => Message, pos: SourcePosition, replace: => Unit = ()): Boolean = {
    if (scala2Mode) {
      migrationWarning(msg, pos)
      replace
    }
    scala2Mode
  }

  /** Is option -language:Scala2 set?
   *  This test is used when we are too early in the pipeline to consider imports.
   */
  def scala2Setting: Boolean =
    ctx.settings.language.value.contains(nme.Scala2.toString)

  /** Refine child based on parent
   *
   *  In child class definition, we have:
   *
   *      class Child[Ts] extends path.Parent[Us] with Es
   *      object Child extends path.Parent[Us] with Es
   *      val child = new path.Parent[Us] with Es           // enum values
   *
   *  Given a parent type `parent` and a child symbol `child`, we infer the prefix
   *  and type parameters for the child:
   *
   *      prefix.child[Vs] <:< parent
   *
   *  where `Vs` are fresh type variables and `prefix` is the symbol prefix with all
   *  non-module and non-package `ThisType` replaced by fresh type variables.
   *
   *  If the subtyping is true, the instantiated type `p.child[Vs]` is
   *  returned. Otherwise, `NoType` is returned.
   */
  def refineUsingParent(parent: Type, child: Symbol)(implicit ctx: Context): Type = {
    if (child.isTerm && child.is(Case, butNot = Module)) return child.termRef // enum vals always match

    // <local child> is a place holder from Scalac, it is hopeless to instantiate it.
    //
    // Quote from scalac (from nsc/symtab/classfile/Pickler.scala):
    //
    //     ...When a sealed class/trait has local subclasses, a single
    //     <local child> class symbol is added as pickled child
    //     (instead of a reference to the anonymous class; that was done
    //     initially, but seems not to work, ...).
    //
    if (child.name == tpnme.LOCAL_CHILD) return child.typeRef

    val childTp = if (child.isTerm) child.termRef else child.typeRef

    instantiate(childTp, parent)(ctx.fresh.setNewTyperState()).dealias
  }

  /** Instantiate type `tp1` to be a subtype of `tp2`
   *
   *  Return the instantiated type if type parameters and this type
   *  in `tp1` can be instantiated such that `tp1 <:< tp2`.
   *
   *  Otherwise, return NoType.
   */
  private def instantiate(tp1: NamedType, tp2: Type)(implicit ctx: Context): Type = {
    /** expose abstract type references to their bounds or tvars according to variance */
    class AbstractTypeMap(maximize: Boolean)(implicit ctx: Context) extends TypeMap {
      def expose(lo: Type, hi: Type): Type =
        if (variance == 0)
          newTypeVar(TypeBounds(lo, hi))
        else if (variance == 1)
          if (maximize) hi else lo
        else
          if (maximize) lo else hi

      def apply(tp: Type): Type = tp match {
        case tp: TypeRef if isBounds(tp.underlying) =>
          val lo = this(tp.info.loBound)
          val hi = this(tp.info.hiBound)
          // See tests/patmat/gadt.scala  tests/patmat/exhausting.scala  tests/patmat/t9657.scala
          val exposed = expose(lo, hi)
          typr.println(s"$tp exposed to =====> $exposed")
          exposed

        case AppliedType(tycon: TypeRef, args) if isBounds(tycon.underlying) =>
          val args2 = args.map(this)
          val lo = this(tycon.info.loBound).applyIfParameterized(args2)
          val hi = this(tycon.info.hiBound).applyIfParameterized(args2)
          val exposed = expose(lo, hi)
          typr.println(s"$tp exposed to =====> $exposed")
          exposed

        case _ =>
          mapOver(tp)
      }
    }

    def minTypeMap(implicit ctx: Context) = new AbstractTypeMap(maximize = false)
    def maxTypeMap(implicit ctx: Context) = new AbstractTypeMap(maximize = true)

    // Fix subtype checking for child instantiation,
    // such that `Foo(Test.this.foo) <:< Foo(Foo.this)`
    // See tests/patmat/i3938.scala
    class RemoveThisMap extends TypeMap {
      var prefixTVar: Type = null
      def apply(tp: Type): Type = tp match {
        case ThisType(tref: TypeRef) if !tref.symbol.isStaticOwner =>
          if (tref.symbol.is(Module))
            TermRef(this(tref.prefix), tref.symbol.sourceModule)
          else if (prefixTVar != null)
            this(tref)
          else {
            prefixTVar = WildcardType  // prevent recursive call from assigning it
            prefixTVar = newTypeVar(TypeBounds.upper(this(tref)))
            prefixTVar
          }
        case tp => mapOver(tp)
      }
    }

    // replace uninstantiated type vars with WildcardType, check tests/patmat/3333.scala
    def instUndetMap(implicit ctx: Context) = new TypeMap {
      def apply(t: Type): Type = t match {
        case tvar: TypeVar if !tvar.isInstantiated => WildcardType(tvar.origin.underlying.bounds)
        case _ => mapOver(t)
      }
    }

    val removeThisType = new RemoveThisMap
    val tvars = tp1.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds) }
    val protoTp1 = removeThisType.apply(tp1).appliedTo(tvars)

    val force = new ForceDegree.Value(
      tvar =>
        !(ctx.typerState.constraint.entry(tvar.origin) `eq` tvar.origin.underlying) ||
        (tvar `eq` removeThisType.prefixTVar),
      minimizeAll = false,
      allowBottom = false
    )

    // If parent contains a reference to an abstract type, then we should
    // refine subtype checking to eliminate abstract types according to
    // variance. As this logic is only needed in exhaustivity check,
    // we manually patch subtyping check instead of changing TypeComparer.
    // See tests/patmat/i3645b.scala
    def parentQualify = tp1.widen.classSymbol.info.parents.exists { parent =>
      implicit val ictx = ctx.fresh.setNewTyperState()
      parent.argInfos.nonEmpty && minTypeMap.apply(parent) <:< maxTypeMap.apply(tp2)
    }

    if (protoTp1 <:< tp2) {
      if (isFullyDefined(protoTp1, force)) protoTp1
      else instUndetMap.apply(protoTp1)
    }
    else {
      val protoTp2 = maxTypeMap.apply(tp2)
      if (protoTp1 <:< protoTp2 || parentQualify) {
        if (isFullyDefined(AndType(protoTp1, protoTp2), force)) protoTp1
        else instUndetMap.apply(protoTp1)
      }
      else {
        typr.println(s"$protoTp1 <:< $protoTp2 = false")
        NoType
      }
    }
  }
}

object TypeOps {
  @sharable var track: Boolean = false // !!!DEBUG

  // TODO: Move other typeops here. It's a bit weird that they are a part of `ctx`

  def nestedPairs(ts: List[Type])(implicit ctx: Context): Type =
    (ts :\ (defn.UnitType: Type))(defn.PairType.appliedTo(_, _))
}
