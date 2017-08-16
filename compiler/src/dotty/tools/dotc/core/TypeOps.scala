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
  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol): Type =
    new AsSeenFromMap(pre, cls).apply(tp)

  /** The TypeMap handling the asSeenFrom */
  class AsSeenFromMap(pre: Type, cls: Symbol) extends ApproximatingTypeMap {

    def apply(tp: Type): Type = {

      /** Map a `C.this` type to the right prefix. If the prefix is unstable, and
       *  the current variance is <= 0, return a range.
       */
      def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type = /*>|>*/ ctx.conditionalTraceIndented(TypeOps.track, s"toPrefix($pre, $cls, $thiscls)") /*<|<*/ {
        if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
          tp
        else pre match {
          case pre: SuperType => toPrefix(pre.thistpe, cls, thiscls)
          case _ =>
            if (thiscls.derivesFrom(cls) && pre.baseType(thiscls).exists)
              if (variance <= 0 && !isLegalPrefix(pre)) range(pre.bottomType, pre)
              else pre
            else if ((pre.termSymbol is Package) && !(thiscls is Package))
              toPrefix(pre.select(nme.PACKAGE), cls, thiscls)
            else
              toPrefix(pre.baseType(cls).normalizedPrefix, cls.owner, thiscls)
      }
      }

    def argForParam(pre: Type, tparam: Symbol): Type = {
      val tparamCls = tparam.owner
      pre.baseType(tparamCls) match {
        case AppliedType(_, allArgs) =>
          var tparams = tparamCls.typeParams
          var args = allArgs
          var idx = 0
          while (tparams.nonEmpty && args.nonEmpty) {
            if (tparams.head.eq(tparam))
              return args.head match {
                case bounds: TypeBounds =>
                  val v = variance
                  if (v > 0) bounds.hi
                  else if (v < 0) bounds.lo
                  else TypeArgRef(pre, cls.typeRef, idx)
                case arg => arg
              }
            tparams = tparams.tail
            args = args.tail
            idx += 1
          }
          throw new AssertionError(ex"$pre contains no matching argument for ${tparam.showLocated} ")
        case OrType(tp1, tp2) => argForParam(tp1, cls) | argForParam(tp2, cls)
        case AndType(tp1, tp2) => argForParam(tp1, cls) & argForParam(tp2, cls)
        case _ => tp
      }
    }

      /*>|>*/ ctx.conditionalTraceIndented(TypeOps.track, s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) /*<|<*/ { // !!! DEBUG
        tp match {
          case tp: NamedType =>
            val sym = tp.symbol
            if (sym.isStatic) tp
            else {
              val pre1 = atVariance(variance max 0)(this(tp.prefix))
              if (Config.newScheme && sym.is(TypeParam)) argForParam(pre1, sym)
              else derivedSelect(tp, pre1)
            }
          case tp: ThisType =>
            toPrefix(pre, cls, tp.cls)
          case _: BoundType | NoPrefix =>
            tp
          case tp: RefinedType =>  //@!!!
            derivedRefinedType(tp, apply(tp.parent), apply(tp.refinedInfo))
          case tp: TypeAlias if tp.variance == 1 => // if variance != 1, need to do the variance calculation
            derivedTypeAlias(tp, apply(tp.alias))
          case _ =>
            mapOver(tp)
        }
      }
    }

    override def reapply(tp: Type) =
      // derived infos have already been subjected to asSeenFrom, hence to need to apply the map again.
      tp
  }

  private def isLegalPrefix(pre: Type)(implicit ctx: Context) =
    pre.isStable || !ctx.phase.isTyper

  /** Implementation of Types#simplified */
  final def simplify(tp: Type, theMap: SimplifyMap): Type = tp match {
    case tp: NamedType =>
      if (tp.symbol.isStatic) tp
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
    case  _: ThisType | _: BoundType | NoPrefix =>
      tp
    case tp: RefinedType => // @!!!
      tp.derivedRefinedType(simplify(tp.parent, theMap), tp.refinedName, simplify(tp.refinedInfo, theMap))
    case tp: TypeAlias =>
      tp.derivedTypeAlias(simplify(tp.alias, theMap))
    case AndType(l, r) =>
      simplify(l, theMap) & simplify(r, theMap)
    case OrType(l, r) =>
      simplify(l, theMap) | simplify(r, theMap)
    case _ =>
      (if (theMap != null) theMap else new SimplifyMap).mapOver(tp)
  }

  class SimplifyMap extends TypeMap {
    def apply(tp: Type) = simplify(tp, this)
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
              def baseTp(cls: ClassSymbol): Type = {
                val base =
                  if (tp1.typeParams.nonEmpty) tp.baseTypeTycon(cls)
                  else tp.baseTypeWithArgs(cls)
                base.mapReduceOr(identity)(mergeRefinedOrApplied)
              }
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

  private def enterArgBinding(formal: Symbol, info: Type, cls: ClassSymbol, decls: Scope) = {
    val lazyInfo = new LazyType { // needed so we do not force `formal`.
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        denot setFlag formal.flags & RetainedTypeArgFlags
        denot.info = info
      }
    }
    val sym = ctx.newSymbol(
      cls, formal.name,
      formal.flagsUNSAFE & RetainedTypeArgFlags | BaseTypeArg | Override,
      lazyInfo,
      coord = cls.coord)
    cls.enter(sym, decls)
  }

  /** Normalize a list of parent types of class `cls` to make sure they are
   *  all (possibly applied) references to classes.
   */
  def normalizeToClassRefs(parents: List[Type], cls: ClassSymbol, decls: Scope): List[Type] = {
    if (Config.newScheme) return parents.mapConserve(_.dealias) // !@@@ track and eliminate usages?
    // println(s"normalizing $parents of $cls in ${cls.owner}") // !!! DEBUG

    // A map consolidating all refinements arising from parent type parameters
    var refinements: SimpleMap[TypeName, Type] = SimpleMap.Empty

    // A map of all formal type parameters of base classes that get refined
    var formals: SimpleMap[TypeName, Symbol] = SimpleMap.Empty // A map of all formal parent parameter

    // Strip all refinements from parent type, populating `refinements` and `formals` maps.
    def normalizeToRef(tp: Type): TypeRef = {
      def fail = throw new TypeError(s"unexpected parent type: $tp")
      tp.dealias match {
        case tp: TypeRef =>
          tp
        case tp @ RefinedType(tp1, name: TypeName, rinfo) =>
          val prevInfo = refinements(name)
          refinements = refinements.updated(name,
            if (prevInfo == null) tp.refinedInfo else prevInfo & tp.refinedInfo)
          formals = formals.updated(name, tp1.typeParamNamed(name))
          normalizeToRef(tp1)
        case tp @ RefinedType(tp1, _: TermName, _) =>
            normalizeToRef(tp1)
        case _: ErrorType =>
          defn.AnyType
        case AnnotatedType(tpe, _) =>
          normalizeToRef(tpe)
        case HKApply(tycon: TypeRef, args) =>
          tycon.info match {
            case TypeAlias(alias) => normalizeToRef(alias.appliedTo(args))
            case _ => fail
          }
        case _ =>
          fail
      }
    }

    val parentRefs = parents map normalizeToRef

    // Enter all refinements into current scope.
    refinements foreachBinding { (name, refinedInfo) =>
      assert(decls.lookup(name) == NoSymbol, // DEBUG
        s"redefinition of ${decls.lookup(name).debugString} in ${cls.showLocated}")
      enterArgBinding(formals(name), refinedInfo, cls, decls)
    }

    if (Config.forwardTypeParams)
      forwardParamBindings(parentRefs, refinements, cls, decls)

    parentRefs
  }

  /** Forward parameter bindings in baseclasses to argument types of
   *  class `cls` if possible.
   *  If there have member definitions
   *
   *     type param v= middle
   *     type middle v= to
   *
   *  where the variances of both alias are the same, then enter a new definition
   *
   *     type param v= to
   *
   *  If multiple forwarders would be generated, join their `to` types with an `&`.
   *
   *  @param cls           The class for which parameter bindings should be forwarded
   *  @param decls	       Its scope
   *  @param parentRefs    The parent type references of `cls`
   *  @param paramBindings The type parameter bindings generated for `cls`
   *
   */
  def forwardParamBindings(parentRefs: List[TypeRef],
                           paramBindings: SimpleMap[TypeName, Type],
                           cls: ClassSymbol, decls: Scope)(implicit ctx: Context) = {

    def forwardRef(argSym: Symbol, from: TypeName, to: TypeAlias) = argSym.info match {
      case info @ TypeAlias(TypeRef(_: ThisType, `from`)) if info.variance == to.variance =>
        val existing = decls.lookup(argSym.name)
        if (existing.exists) existing.info = existing.info & to
        else enterArgBinding(argSym, to, cls, decls)
      case _ =>
    }

    def forwardRefs(from: TypeName, to: Type) = to match {
      case to: TypeAlias =>
        for (pref <- parentRefs) {
          def forward()(implicit ctx: Context): Unit =
            for (argSym <- pref.decls)
              if (argSym is BaseTypeArg) forwardRef(argSym, from, to)
          pref.info match {
            case info: TempClassInfo => info.addSuspension(implicit ctx => forward())
            case _ => forward()
          }
        }
      case _ =>
    }

    paramBindings.foreachBinding(forwardRefs)
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
