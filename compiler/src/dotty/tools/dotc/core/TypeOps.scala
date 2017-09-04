package dotty.tools
package dotc
package core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._
import SymDenotations._, Denotations.SingleDenotation
import config.Printers.typr
import util.Positions._
import NameOps._
import NameKinds.DepParamName
import Decorators._
import StdNames._
import Annotations._
import annotation.tailrec
import config.Config
import util.{SimpleMap, Property}
import collection.mutable
import ast.tpd._

trait TypeOps { this: Context => // TODO: Make standalone object.

  /** The type `tp` as seen from prefix `pre` and owner `cls`. See the spec
   *  for what this means.
   */
  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol): Type = tp match {
    case tp: NamedType =>
      if (tp.symbol.isStatic) tp
      else
        tp.derivedSelect(asSeenFrom(tp.prefix, pre, cls)) match {
          case tp1: TypeArgRef => tp1.underlying.hiBound
          case tp1 => tp1
        }
    case tp: ThisType => toPrefix(tp, pre, cls, tp.cls, 1)
    case _: BoundType => tp
    case _ => new AsSeenFromMap(pre, cls, 1).mapOver2(tp)
  }

  /** Map a `C.this` type to the right prefix. If the prefix is unstable, and
    *  the current variance is <= 0, return a range.
    */
  def toPrefix(tp: Type, pre: Type, cls: Symbol, thiscls: ClassSymbol, variance: Int): Type = /*>|>*/ ctx.conditionalTraceIndented(TypeOps.track, s"toPrefix($pre, $cls, $thiscls)") /*<|<*/ {
    if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
      tp
    else pre match {
      case pre: SuperType => toPrefix(tp, pre.thistpe, cls, thiscls, variance)
      case _ =>
        if (thiscls.derivesFrom(cls) && pre.baseType(thiscls).exists)
          if (variance > 0 || isLegalPrefix(pre)) pre
          else new AsSeenFromMap(pre, cls, variance).range(pre.bottomType, pre)
        else if ((pre.termSymbol is Package) && !(thiscls is Package))
          toPrefix(tp, pre.select(nme.PACKAGE), cls, thiscls, variance)
        else
          toPrefix(tp, pre.baseType(cls).normalizedPrefix, cls.owner, thiscls, variance)
    }
  }

  /** The TypeMap handling the asSeenFrom */
  class AsSeenFromMap(pre: Type, cls: Symbol, v: Int) extends ApproximatingTypeMap {
    variance = v

    def apply(tp: Type): Type =
      /*>|> ctx.conditionalTraceIndented(TypeOps.track, s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) <|<*/ { // !!! DEBUG
        tp match {
          case tp: NamedType =>
            if (tp.symbol.isStatic) tp
            else derivedSelect(tp, atVariance(variance max 0)(this(tp.prefix)))
          case tp: ThisType => toPrefix(tp, pre, cls, tp.cls, variance)
          case _: BoundType => tp
          case _ => mapOver2(tp)
        }
      }

    // Specialize mapOver2 to get monomorphic dispatch for handling AppliedTypes
    override def mapOver2(tp: Type) = tp match {
      case tp: AppliedType =>
        def mapArgs(args: List[Type], tparams: List[ParamInfo]): List[Type] = args match {
          case arg :: otherArgs =>
            val arg1 = arg match {
              case arg: TypeBounds => this(arg)
              case arg => atVariance(variance * tparams.head.paramVariance)(this(arg))
            }
            val otherArgs1 = mapArgs(otherArgs, tparams.tail)
            if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
            else arg1 :: otherArgs1
          case nil =>
            nil
        }
        derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args, tp.typeParams))
      case _ =>
        mapOver3(tp)
    }

    override def reapply(tp: Type) =
      // derived infos have already been subjected to asSeenFrom, hence to need to apply the map again.
      tp
  }

  private def isLegalPrefix(pre: Type)(implicit ctx: Context) =
    pre.isStable || !ctx.phase.isTyper

  /** Approximate a type `tp` with a type that does not contain skolem types. */
  object deskolemize extends ApproximatingTypeMap {
    def apply(tp: Type) = /*ctx.traceIndented(i"deskolemize($tp) at $variance", show = true)*/ {
      tp match {
        case tp: SkolemType => range(tp.bottomType, atVariance(1)(apply(tp.info)))
        case _ => mapOver(tp)
      }
    }
  }

  /** Implementation of Types#simplified */
  final def simplify(tp: Type): Type = tp match {
    case tp: NamedType => simplifyNamed(tp)
    case _: ThisType => tp
    case _ => new SimplifyMap().mapOver2(tp)
  }

  def simplifyNamed(tp: NamedType) =
    if (tp.symbol.isStatic) tp
    else tp.derivedSelect(simplify(tp.prefix)) match {
      case tp1: NamedType if tp1.denotationIsCurrent => tp1.reduceProjection
      case tp1 => tp1
    }

  private class SimplifyMap extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => simplifyNamed(tp)
      case _: ThisType => tp
      case _ => mapOver2(tp)
    }

    // Specialize mapOver2 to get monomorphic dispatch for handling AppliedTypes
    override def mapOver2(tp: Type) = tp match {
      case tp: AppliedType =>
        def mapArgs(args: List[Type]): List[Type] = args match {
          case arg :: otherArgs =>
            val arg1 = this(arg)
            val otherArgs1 = mapArgs(otherArgs)
            if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
            else arg1 :: otherArgs1
          case nil =>
            nil
        }
        derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args))
      case _ =>
        mapOver3(tp)
    }

    override def mapOver3(tp: Type) = tp match {
      case tp: TypeParamRef =>
        if (tp.paramName.is(DepParamName)) {
          val bounds = ctx.typeComparer.bounds(tp)
          if (bounds.lo.isRef(defn.NothingClass)) bounds.hi else bounds.lo
        }
        else {
          val tvar = typerState.constraint.typeVarOfParam(tp)
          if (tvar.exists) tvar else tp
        }
      case _: BoundType | NoPrefix | NoType =>
        tp
      case AndType(l, r) if !ctx.mode.is(Mode.Type) => // TODO: Drop all simplifications if mode isType?
        this(l) & this(r)
      case OrType(l, r) if !ctx.mode.is(Mode.Type) =>
        this(l) | this(r)
      case _ =>
        super.mapOver3(tp)
    }
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
   *  we approximate `A | B` by `C[A | B] with D`
   */
  def orDominator(tp: Type): Type = {

    /** a faster version of cs1 intersect cs2 */
    def intersect(cs1: List[ClassSymbol], cs2: List[ClassSymbol]): List[ClassSymbol] = {
      val cs2AsSet = new util.HashSet[ClassSymbol](100)
      cs2.foreach(cs2AsSet.addEntry)
      cs1.filter(cs2AsSet.contains)
    }

    /** The minimal set of classes in `cs` which derive all other classes in `cs` */
    def dominators(cs: List[ClassSymbol], accu: List[ClassSymbol]): List[ClassSymbol] = (cs: @unchecked) match {
      case c :: rest =>
        val accu1 = if (accu exists (_ derivesFrom c)) accu else c :: accu
        if (cs == c.baseClasses) accu1 else dominators(rest, accu1)
      case Nil => // this case can happen because after erasure we do not have a top class anymore
        assert(ctx.erasedTypes)
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
        case tp1 @ TypeRef(pre1, name1) =>
          tp2 match {
            case tp2 @ TypeRef(pre2, `name1`) =>
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

      tp1 match {
        case tp1: RecType =>
          tp1.rebind(approximateOr(tp1.parent, tp2))
        case tp1: TypeProxy if !isClassRef(tp1) =>
          orDominator(tp1.superType | tp2)
        case _ =>
          tp2 match {
            case tp2: RecType =>
              tp2.rebind(approximateOr(tp1, tp2.parent))
            case tp2: TypeProxy if !isClassRef(tp2) =>
              orDominator(tp1 | tp2.superType)
            case _ =>
              val commonBaseClasses = tp.mapReduceOr(_.baseClasses)(intersect)
              val doms = dominators(commonBaseClasses, Nil)
              def baseTp(cls: ClassSymbol): Type =
                tp.baseType(cls).mapReduceOr(identity)(mergeRefinedOrApplied)
              doms.map(baseTp).reduceLeft(AndType.apply)
          }
      }
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
      case pkgCls: PackageClassDenotation if !(tpe.symbol.maybeOwner is Package) =>
        tpe.derivedSelect(pkgCls.packageObj.valRef)
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
   */
  def boundsViolations(args: List[Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type)(implicit ctx: Context): List[BoundsViolation] = {
    val argTypes = args.tpes
    val violations = new mutable.ListBuffer[BoundsViolation]
    for ((arg, bounds) <- args zip boundss) {
      def checkOverlapsBounds(lo: Type, hi: Type): Unit = {
        //println(i"instantiating ${bounds.hi} with $argTypes")
        //println(i" = ${instantiate(bounds.hi, argTypes)}")
        val hiBound = instantiate(bounds.hi, argTypes.mapConserve(_.bounds.hi))
        val loBound = instantiate(bounds.lo, argTypes.mapConserve(_.bounds.lo))
          // Note that argTypes can contain a TypeBounds type for arguments that are
          // not fully determined. In that case we need to check against the hi bound of the argument.
        if (!(lo <:< hiBound)) violations += ((arg, "upper", hiBound))
        if (!(loBound <:< hi)) violations += ((arg, "lower", bounds.lo))
      }
      arg.tpe match {
        case TypeBounds(lo, hi) => checkOverlapsBounds(lo, hi)
        case tp => checkOverlapsBounds(tp, tp)
      }
    }
    violations.toList
  }

  /** Is `feature` enabled in class `owner`?
   *  This is the case if one of the following two alternatives holds:
   *
   *  1. The feature is imported by a named import
   *
   *       import owner.feature
   *
   *  (the feature may be bunched with others, or renamed, but wildcard imports
   *  don't count).
   *
   *  2. The feature is enabled by a compiler option
   *
   *       - language:<prefix>feature
   *
   *  where <prefix> is the full name of the owner followed by a "." minus
   *  the prefix "dotty.language.".
   */
  def featureEnabled(owner: ClassSymbol, feature: TermName): Boolean = {
    def toPrefix(sym: Symbol): String =
      if (!sym.exists || (sym eq defn.LanguageModuleClass)) ""
      else toPrefix(sym.owner) + sym.name + "."
    def featureName = toPrefix(owner) + feature
    def hasImport(implicit ctx: Context): Boolean = {
      if (ctx.importInfo == null || (ctx.importInfo.site.widen.typeSymbol ne owner)) false
      else if (ctx.importInfo.excluded.contains(feature)) false
      else if (ctx.importInfo.originals.contains(feature)) true
      else {
        var c = ctx.outer
        while (c.importInfo eq ctx.importInfo) c = c.outer
        hasImport(c)
      }
    }
    def hasOption = ctx.base.settings.language.value exists (s => s == featureName || s == "_")
    hasImport(ctx.withPhase(ctx.typerPhase)) || hasOption
  }

  /** Is auto-tupling enabled? */
  def canAutoTuple =
    !featureEnabled(defn.LanguageModuleClass, nme.noAutoTupling)

  def scala2Mode =
    featureEnabled(defn.LanguageModuleClass, nme.Scala2)

  def dynamicsEnabled =
    featureEnabled(defn.LanguageModuleClass, nme.dynamics)

  def testScala2Mode(msg: => String, pos: Position, rewrite: => Unit = ()) = {
    if (scala2Mode) {
      migrationWarning(msg, pos)
      rewrite
    }
    scala2Mode
  }
}

object TypeOps {
  @sharable var track = false // !!!DEBUG

  /** When a property with this key is set in a context, it limit the number
   *  of recursive member searches. If the limit is reached, findMember returns
   *  NoDenotation.
   */
  val findMemberLimit = new Property.Key[Unit]
}
