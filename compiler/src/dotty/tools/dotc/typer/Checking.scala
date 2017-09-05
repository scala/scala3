package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._
import Types._
import Flags._
import Denotations._
import Names._
import StdNames._
import NameOps._
import Symbols._
import Trees._
import ProtoTypes._
import Constants._
import Scopes._
import CheckRealizable._
import ErrorReporting.errorTree

import annotation.unchecked
import util.Positions._
import util.{SimpleMap, Stats}
import util.common._
import transform.SymUtils._
import Decorators._
import Uniques._
import ErrorReporting.{err, errorType}
import config.Printers.typr

import collection.mutable
import SymDenotations.NoCompleter
import dotty.tools.dotc.reporting.diagnostic.{ErrorMessageID, Message}
import dotty.tools.dotc.reporting.diagnostic.messages._
import dotty.tools.dotc.transform.ValueClasses._

object Checking {
  import tpd._

  /** A general checkBounds method that can be used for TypeApply nodes as
   *  well as for AppliedTypeTree nodes. Also checks that type arguments to
   *  *-type parameters are fully applied.
   */
  def checkBounds(args: List[tpd.Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type)(implicit ctx: Context): Unit = {
    (args, boundss).zipped.foreach { (arg, bound) =>
      if (!bound.isHK && arg.tpe.isHK)
        // see MissingTypeParameterFor
        ctx.error(ex"missing type parameter(s) for $arg", arg.pos)
    }
    for ((arg, which, bound) <- ctx.boundsViolations(args, boundss, instantiate))
      ctx.error(
          DoesNotConformToBound(arg.tpe, which, bound)(err),
          arg.pos.focus)
  }

  /** Check that type arguments `args` conform to corresponding bounds in `tl`
   *  Note: This does not check the bounds of AppliedTypeTrees. These
   *  are handled by method checkBounds in FirstTransform
   */
  def checkBounds(args: List[tpd.Tree], tl: TypeLambda)(implicit ctx: Context): Unit =
    checkBounds(args, tl.paramInfos, _.substParams(tl, _))

  /** Check applied type trees for well-formedness. This means
   *   - all arguments are within their corresponding bounds
   *   - if type is a higher-kinded application with wildcard arguments,
   *     check that it or one of its supertypes can be reduced to a normal application.
   *     Unreducible applications correspond to general existentials, and we
   *     cannot handle those.
   */
  def checkAppliedType(tree: AppliedTypeTree)(implicit ctx: Context) = {
    val AppliedTypeTree(tycon, args) = tree
    // If `args` is a list of named arguments, return corresponding type parameters,
    // otherwise return type parameters unchanged
    val tparams = tycon.tpe.typeParams
    def argNamed(tparam: ParamInfo) = args.find {
      case NamedArg(name, _) => name == tparam.paramName
      case _ => false
    }.getOrElse(TypeTree(tparam.paramRef))
    val orderedArgs = if (hasNamedArg(args)) tparams.map(argNamed) else args
    val bounds = tparams.map(_.paramInfoAsSeenFrom(tycon.tpe).bounds)
    def instantiate(bound: Type, args: List[Type]) =
      HKTypeLambda.fromParams(tparams, bound).appliedTo(args)
    checkBounds(orderedArgs, bounds, instantiate)

    def checkWildcardHKApply(tp: Type, pos: Position): Unit = tp match {
      case tp @ HKApply(tycon, args) if args.exists(_.isInstanceOf[TypeBounds]) =>
        tycon match {
          case tycon: TypeLambda =>
            ctx.errorOrMigrationWarning(
              ex"unreducible application of higher-kinded type $tycon to wildcard arguments",
              pos)
          case _ =>
            checkWildcardHKApply(tp.superType, pos)
        }
      case _ =>
    }
    def checkValidIfHKApply(implicit ctx: Context): Unit =
      checkWildcardHKApply(tycon.tpe.appliedTo(args.map(_.tpe)), tree.pos)
    checkValidIfHKApply(ctx.addMode(Mode.AllowLambdaWildcardApply))
  }

  /** Check that kind of `arg` has the same outline as the kind of paramBounds.
   *  E.g. if `paramBounds` has kind * -> *, `arg` must have that kind as well,
   *  and analogously for all other kinds. This kind checking does not take into account
   *  variances or bounds. The more detailed kind checking is done as part of checkBounds in PostTyper.
   *  The purpose of preCheckKind is to do a rough test earlier in Typer,
   *  in order to prevent scenarios that lead to self application of
   *  types. Self application needs to be avoided since it can lead to stack overflows.
   *  Test cases are neg/i2771.scala and neg/i2771b.scala.
   */
  def preCheckKind(arg: Tree, paramBounds: TypeBounds)(implicit ctx: Context): Tree =
    if (arg.tpe.widen.isRef(defn.NothingClass) || arg.tpe.hasSameKindAs(paramBounds.hi)) arg
    else errorTree(arg, em"Type argument ${arg.tpe} has not the same kind as its bound $paramBounds")

  def preCheckKinds(args: List[Tree], paramBoundss: List[TypeBounds])(implicit ctx: Context): List[Tree] = {
    val args1 = args.zipWithConserve(paramBoundss)(preCheckKind)
    args1 ++ args.drop(paramBoundss.length)
      // add any arguments that do not correspond to a parameter back,
      // so the wrong number of parameters is reported afterwards.
  }

  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  def checkInstantiable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.is(AbstractOrTrait))
          ctx.error(CantInstantiateAbstractClassOrTrait(cls, isTrait = cls.is(Trait)), pos)
        if (!cls.is(Module)) {
          // Create a synthetic singleton type instance, and check whether
          // it conforms to the self type of the class as seen from that instance.
          val stp = SkolemType(tp)
          val selfType = tref.givenSelfType.asSeenFrom(stp, cls)
          if (selfType.exists && !(stp <:< selfType))
            ctx.error(DoesNotConformToSelfTypeCantBeInstantiated(tp, selfType), pos)
        }
      case _ =>
    }

  /** Check that type `tp` is realizable. */
  def checkRealizable(tp: Type, pos: Position)(implicit ctx: Context): Unit = {
    val rstatus = realizability(tp)
    if (rstatus ne Realizable)
      ctx.errorOrMigrationWarning(em"$tp is not a legal path\n since it${rstatus.msg}", pos)
  }

  /** A type map which checks that the only cycles in a type are F-bounds
   *  and that protects all F-bounded references by LazyRefs.
   */
  class CheckNonCyclicMap(sym: Symbol, reportErrors: Boolean)(implicit ctx: Context) extends TypeMap {

    /** Set of type references whose info is currently checked */
    private val locked = mutable.Set[TypeRef]()

    /** Are cycles allowed within nested refinedInfos of currently checked type? */
    private var nestedCycleOK = false

    /** Are cycles allowed within currently checked type? */
    private var cycleOK = false

    /** A diagnostic output string that indicates the position of the last
     *  part of a type bounds checked by checkInfo. Possible choices:
     *  alias, lower bound, upper bound.
     */
    var where: String = ""

    /** The last type top-level type checked when a CyclicReference occurs. */
    var lastChecked: Type = NoType

    /** Check info `tp` for cycles. Throw CyclicReference for illegal cycles,
     *  break direct cycle with a LazyRef for legal, F-bounded cycles.
     */
    def checkInfo(tp: Type): Type = tp match {
      case tp @ TypeAlias(alias) =>
        try tp.derivedTypeAlias(apply(alias))
        finally {
          where = "alias"
          lastChecked = alias
        }
      case tp @ TypeBounds(lo, hi) =>
        val lo1 = try apply(lo) finally {
          where = "lower bound"
          lastChecked = lo
        }
        val saved = nestedCycleOK
        nestedCycleOK = true
        try tp.derivedTypeBounds(lo1, apply(hi))
        finally {
          nestedCycleOK = saved
          where = "upper bound"
          lastChecked = hi
        }
      case _ =>
        tp
    }

    private def apply(tp: Type, cycleOK: Boolean, nestedCycleOK: Boolean): Type = {
      val savedCycleOK = this.cycleOK
      val savedNestedCycleOK = this.nestedCycleOK
      this.cycleOK = cycleOK
      this.nestedCycleOK = nestedCycleOK
      try apply(tp)
      finally {
        this.cycleOK = savedCycleOK
        this.nestedCycleOK = savedNestedCycleOK
      }
    }

    def apply(tp: Type): Type = tp match {
      case tp: TermRef =>
        this(tp.info)
        mapOver(tp)
      case tp @ RefinedType(parent, name, rinfo) =>
        tp.derivedRefinedType(this(parent), name, this(rinfo, nestedCycleOK, nestedCycleOK))
      case tp: RecType =>
        tp.rebind(this(tp.parent))
      case tp @ HKApply(tycon, args) =>
        tp.derivedAppliedType(this(tycon), args.map(this(_, nestedCycleOK, nestedCycleOK)))
      case tp @ TypeRef(pre, name) =>
        try {
          // A prefix is interesting if it might contain (transitively) a reference
          // to symbol `sym` itself. We only check references with interesting
          // prefixes for cycles. This pruning is done in order not to force
          // global symbols when doing the cyclicity check.
          def isInteresting(prefix: Type): Boolean = prefix.stripTypeVar match {
            case NoPrefix => true
            case prefix: ThisType => sym.owner.isClass && prefix.cls.isContainedIn(sym.owner)
            case prefix: NamedType =>
              (!sym.is(Private) && prefix.derivesFrom(sym.owner)) ||
              (!prefix.symbol.isStaticOwner && isInteresting(prefix.prefix))
            case SuperType(thistp, _) => isInteresting(thistp)
            case AndType(tp1, tp2) => isInteresting(tp1) || isInteresting(tp2)
            case OrType(tp1, tp2) => isInteresting(tp1) && isInteresting(tp2)
            case _: RefinedOrRecType | _: HKApply => true
            case _ => false
          }
          if (isInteresting(pre)) {
            val pre1 = this(pre, false, false)
            if (locked.contains(tp) || tp.symbol.infoOrCompleter == NoCompleter)
              throw CyclicReference(tp.symbol)
            locked += tp
            try checkInfo(tp.info)
            finally locked -= tp
            if (pre1 eq pre) tp else tp.newLikeThis(pre1)
          }
          else tp
        } catch {
          case ex: CyclicReference =>
            ctx.debuglog(i"cycle detected for $tp, $nestedCycleOK, $cycleOK")
            if (cycleOK) LazyRef(_ => tp)
            else if (reportErrors) throw ex
            else tp
        }
      case _ => mapOver(tp)
    }
  }

  /** Check that `info` of symbol `sym` is not cyclic.
   *  @pre     sym is not yet initialized (i.e. its type is a Completer).
   *  @return  `info` where every legal F-bounded reference is proctected
   *                  by a `LazyRef`, or `ErrorType` if a cycle was detected and reported.
   */
  def checkNonCyclic(sym: Symbol, info: Type, reportErrors: Boolean)(implicit ctx: Context): Type = {
    val checker = new CheckNonCyclicMap(sym, reportErrors)(ctx.addMode(Mode.CheckCyclic))
    try checker.checkInfo(info)
    catch {
      case ex: CyclicReference =>
        if (reportErrors) {
          errorType(i"illegal cyclic reference: ${checker.where} ${checker.lastChecked} of $sym refers back to the type itself", sym.pos)
        }
        else info
    }
  }

  /** Check that refinement satisfies the following two conditions
   *  1. No part of it refers to a symbol that's defined in the same refinement
   *     at a textually later point.
   *  2. All references to the refinement itself via `this` are followed by
   *     selections.
   *  Note: It's not yet clear what exactly we want to allow and what we want to rule out.
   *  This depends also on firming up the DOT calculus. For the moment we only issue
   *  deprecated warnings, not errors.
   */
  def checkRefinementNonCyclic(refinement: Tree, refineCls: ClassSymbol, seen: mutable.Set[Symbol])
    (implicit ctx: Context): Unit = {
    def flag(what: String, tree: Tree) =
      ctx.deprecationWarning(i"$what reference in refinement is deprecated", tree.pos)
    def forwardRef(tree: Tree) = flag("forward", tree)
    def selfRef(tree: Tree) = flag("self", tree)
    val checkTree = new TreeAccumulator[Unit] {
      def checkRef(tree: Tree, sym: Symbol) =
        if (sym.maybeOwner == refineCls && !seen(sym)) forwardRef(tree)
      def apply(x: Unit, tree: Tree)(implicit ctx: Context) = tree match {
        case tree: MemberDef =>
          foldOver(x, tree)
          seen += tree.symbol
        case tree @ Select(This(_), _) =>
          checkRef(tree, tree.symbol)
        case tree: RefTree =>
          checkRef(tree, tree.symbol)
          foldOver(x, tree)
        case tree: This =>
          selfRef(tree)
        case tree: TypeTree =>
          val checkType = new TypeAccumulator[Unit] {
            def apply(x: Unit, tp: Type): Unit = tp match {
              case tp: NamedType =>
                checkRef(tree, tp.symbol)
                tp.prefix match {
                  case pre: ThisType =>
                  case pre => foldOver(x, pre)
                }
              case tp: ThisType if tp.cls == refineCls =>
                selfRef(tree)
              case _ =>
                foldOver(x, tp)
            }
          }
          checkType((), tree.tpe)
        case _ =>
          foldOver(x, tree)
      }
    }
    checkTree((), refinement)
  }

  /** Check that symbol's definition is well-formed. */
  def checkWellFormed(sym: Symbol)(implicit ctx: Context): Unit = {
    def fail(msg: Message) = ctx.error(msg, sym.pos)

    def checkWithDeferred(flag: FlagSet) =
      if (sym.is(flag))
        fail(AbstractMemberMayNotHaveModifier(sym, flag))
    def checkNoConflict(flag1: FlagSet, flag2: FlagSet) =
      if (sym.is(allOf(flag1, flag2)))
        fail(i"illegal combination of modifiers: `$flag1` and `$flag2` for: $sym")
    def checkApplicable(flag: FlagSet, ok: Boolean) =
      if (!ok && !sym.is(Synthetic))
        fail(i"modifier `$flag` is not allowed for this definition")

    if (sym.is(ImplicitCommon)) {
      if (sym.owner.is(Package))
        fail(TopLevelCantBeImplicit(sym))
      if (sym.isType)
        fail(TypesAndTraitsCantBeImplicit(sym))
    }
    if (!sym.isClass && sym.is(Abstract))
      fail(OnlyClassesCanBeAbstract(sym))
    if (sym.is(AbsOverride) && !sym.owner.is(Trait))
      fail(AbstractOverrideOnlyInTraits(sym))
    if (sym.is(Trait) && sym.is(Final))
      fail(TraitsMayNotBeFinal(sym))
    if (sym.hasAnnotation(defn.NativeAnnot)) {
      if (!sym.is(Deferred))
        fail(NativeMembersMayNotHaveImplementation(sym))
    }
    else if (sym.is(Deferred, butNot = Param) && !sym.isType && !sym.isSelfSym) {
      if (!sym.owner.isClass || sym.owner.is(Module) || sym.owner.isAnonymousClass)
        fail(OnlyClassesCanHaveDeclaredButUndefinedMembers(sym))
      checkWithDeferred(Private)
      checkWithDeferred(Final)
      checkWithDeferred(Inline)
    }
    if (sym.isValueClass && sym.is(Trait) && !sym.isRefinementClass)
      fail(CannotExtendAnyVal(sym))
    checkNoConflict(Final, Sealed)
    checkNoConflict(Private, Protected)
    checkNoConflict(Abstract, Override)
    checkNoConflict(Lazy, Inline)
    if (sym.is(Inline)) checkApplicable(Inline, sym.isTerm && !sym.is(Mutable | Module))
    if (sym.is(Lazy)) checkApplicable(Lazy, !sym.is(Method | Mutable))
    if (sym.isType && !sym.is(Deferred))
      for (cls <- sym.allOverriddenSymbols.filter(_.isClass)) {
        fail(CannotHaveSameNameAs(sym, cls, CannotHaveSameNameAs.CannotBeOverridden))
        sym.setFlag(Private) // break the overriding relationship by making sym Private
      }
  }

  /** Check the type signature of the symbol `M` defined by `tree` does not refer
   *  to a private type or value which is invisible at a point where `M` is still
   *  visible.
   *
   *  As an exception, we allow references to type aliases if the underlying
   *  type of the alias is not a leak, and if `sym` is not a type. The rationale
   *  for this is that the inferred type of a term symbol might contain leaky
   *  aliases which should be removed (see leak-inferred.scala for an example),
   *  but a type symbol definition will not contain leaky aliases unless the
   *  user wrote them, so we can ask the user to change his definition. The more
   *  practical reason for not transforming types is that `checkNoPrivateLeaks`
   *  can force a lot of denotations, and this restriction means that we never
   *  need to run `TypeAssigner#avoidPrivateLeaks` on type symbols when
   *  unpickling, which avoids some issues related to forcing order.
   *
   *  See i997.scala for negative tests, and i1130.scala for a case where it
   *  matters that we transform leaky aliases away.
   *
   *  @return The `info` of `sym`, with problematic aliases expanded away.
   */
  def checkNoPrivateLeaks(sym: Symbol, pos: Position)(implicit ctx: Context): Type = {
    class NotPrivate extends TypeMap {
      var errors: List[() => String] = Nil

      def accessBoundary(sym: Symbol): Symbol =
        if (sym.is(Private) || !sym.owner.isClass) sym.owner
        else if (sym.privateWithin.exists) sym.privateWithin
        else if (sym.is(Package)) sym
        else accessBoundary(sym.owner)

      val symBoundary = accessBoundary(sym)

      /** Is `other` leaked outside its access boundary ?
       *  @pre  The signature of `sym` refers to `other`
       */
      def isLeaked(other: Symbol) =
        other.is(Private) && {
          val otherBoundary = other.owner
          val otherLinkedBoundary = otherBoundary.linkedClass
          !(symBoundary.isContainedIn(otherBoundary) ||
            otherLinkedBoundary.exists && symBoundary.isContainedIn(otherLinkedBoundary))
        }

      def apply(tp: Type): Type = tp match {
        case tp: NamedType =>
          val prevErrors = errors
          var tp1 =
            if (isLeaked(tp.symbol)) {
              errors =
                (() => em"non-private $sym refers to private ${tp.symbol}\n in its type signature ${sym.info}") :: errors
              tp
            }
            else mapOver(tp)
          if ((errors ne prevErrors) && !sym.isType && tp.info.isAlias) {
            // try to dealias to avoid a leak error
            val savedErrors = errors
            errors = prevErrors
            val tp2 = apply(tp.superType)
            if (errors eq prevErrors) tp1 = tp2
            else errors = savedErrors
          }
          tp1
        case tp: ClassInfo =>
          tp.derivedClassInfo(
            prefix = apply(tp.prefix),
            classParents =
              tp.parentsWithArgs.map { p =>
                apply(p).underlyingClassRef(refinementOK = false) match {
                  case ref: TypeRef => ref
                  case _ => defn.ObjectType // can happen if class files are missing
                }
              }
            )
        case _ =>
          mapOver(tp)
      }
    }
    val notPrivate = new NotPrivate
    val info = notPrivate(sym.info)
    notPrivate.errors.foreach(error => ctx.errorOrMigrationWarning(error(), pos))
    info
  }

  /** Verify classes extending AnyVal meet the requirements */
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context) = {
    def checkValueClassMember(stat: Tree) = stat match {
      case _: TypeDef if stat.symbol.isClass =>
        ctx.error(ValueClassesMayNotDefineInner(clazz, stat.symbol), stat.pos)
      case _: ValDef if !stat.symbol.is(ParamAccessor) =>
        ctx.error(ValueClassesMayNotDefineNonParameterField(clazz, stat.symbol), stat.pos)
      case _: DefDef if stat.symbol.isConstructor =>
        ctx.error(ValueClassesMayNotDefineASecondaryConstructor(clazz, stat.symbol), stat.pos)
      case _: MemberDef | _: Import | EmptyTree =>
      // ok
      case _ =>
        ctx.error(ValueClassesMayNotContainInitalization(clazz), stat.pos)
    }
    if (isDerivedValueClass(clazz)) {
      if (clazz.is(Trait))
        ctx.error(CannotExtendAnyVal(clazz), clazz.pos)
      if (clazz.is(Abstract))
        ctx.error(ValueClassesMayNotBeAbstract(clazz), clazz.pos)
      if (!clazz.isStatic)
        ctx.error(ValueClassesMayNotBeContainted(clazz), clazz.pos)
      if (isCyclic(clazz.asClass))
        ctx.error(ValueClassesMayNotWrapItself(clazz), clazz.pos)
      else {
        val clParamAccessors = clazz.asClass.paramAccessors.filter { param =>
          param.isTerm && !param.is(Flags.Accessor)
        }
        clParamAccessors match {
          case param :: params =>
            if (param.is(Mutable))
              ctx.error(ValueClassParameterMayNotBeAVar(clazz, param), param.pos)
            if (param.info.isPhantom)
              ctx.error("value class first parameter must not be phantom", param.pos)
            else {
              for (p <- params if !p.info.isPhantom)
                ctx.error("value class can only have one non phantom parameter", p.pos)
            }
          case Nil =>
            ctx.error(ValueClassNeedsOneValParam(clazz), clazz.pos)
        }
      }
      stats.foreach(checkValueClassMember)
    }
  }
}

trait Checking {

  import tpd._

  def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(implicit ctx: Context): Type =
    Checking.checkNonCyclic(sym, info, reportErrors)

  /** Check that Java statics and packages can only be used in selections.
   */
  def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = {
    if (!proto.isInstanceOf[SelectionProto] && !proto.isInstanceOf[ApplyingProto]) {
      val sym = tree.tpe.termSymbol
      // The check is avoided inside Java compilation units because it always fails
      // on the singleton type Module.type.
      if ((sym is Package) || ((sym is JavaModule) && !ctx.compilationUnit.isJava)) ctx.error(em"$sym is not a value", tree.pos)
    }
    tree
  }

  /** Check that type `tp` is stable. */
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(ex"$tp is not stable", pos)

  /** Check that all type members of `tp` have realizable bounds */
  def checkRealizableBounds(tp: Type, pos: Position)(implicit ctx: Context): Unit = {
    val rstatus = boundsRealizability(tp)
    if (rstatus ne Realizable)
      ctx.error(ex"$tp cannot be instantiated since it${rstatus.msg}", pos)
  }

 /**  Check that `tp` is a class type.
  *   Also, if `traitReq` is true, check that `tp` is a trait.
  *   Also, if `stablePrefixReq` is true and phase is not after RefChecks,
  *   check that class prefix is stable.
   *  @return  `tp` itself if it is a class or trait ref, ObjectType if not.
   */
  def checkClassType(tp: Type, pos: Position, traitReq: Boolean, stablePrefixReq: Boolean)(implicit ctx: Context): Type =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        if (traitReq && !(tref.symbol is Trait)) ctx.error(ex"$tref is not a trait", pos)
        if (stablePrefixReq && ctx.phase <= ctx.refchecksPhase) checkStable(tref.prefix, pos)
        tp
      case _ =>
        ctx.error(ex"$tp is not a class type", pos)
        defn.ObjectType
  }

  /** Check that a non-implicit parameter making up the first parameter section of an
   *  implicit conversion is not a singleton type.
   */
  def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = vparamss match {
    case (vparam :: Nil) :: _ if !(vparam.symbol is Implicit) =>
      if (vparam.tpt.tpe.isInstanceOf[SingletonType])
        ctx.error(s"implicit conversion may not have a parameter of singleton type", vparam.tpt.pos)
    case _ =>
  }

  /** Check that any top-level type arguments in this type are feasible, i.e. that
   *  their lower bound conforms to their upper bound. If a type argument is
   *  infeasible, issue and error and continue with upper bound.
   */
  def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp match {
    case tp: RefinedType =>
      tp.derivedRefinedType(tp.parent, tp.refinedName, checkFeasible(tp.refinedInfo, pos, where))
    case tp: RecType =>
      tp.rebind(tp.parent)
    case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
      ctx.error(ex"no type exists between low bound $lo and high bound $hi$where", pos)
      TypeAlias(hi)
    case _ =>
      tp
  }

  /** Check that `tree` is a pure expression of constant type */
  def checkInlineConformant(tree: Tree, what: => String)(implicit ctx: Context): Unit =
    tree.tpe match {
      case tp: TermRef if tp.symbol.is(InlineParam) => // ok
      case tp => tp.widenTermRefExpr match {
        case tp: ConstantType if isPureExpr(tree) => // ok
        case tp if defn.isFunctionType(tp) && isPureExpr(tree) => // ok
        case _ =>
          if (!ctx.erasedTypes) ctx.error(em"$what must be a constant expression or a function", tree.pos)
      }
    }

  /** Check that class does not define same symbol twice */
  def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = {
    val seen = new mutable.HashMap[Name, List[Symbol]] {
      override def default(key: Name) = Nil
    }
    typr.println(i"check no double defs $cls")

    def checkDecl(decl: Symbol): Unit = {
      for (other <- seen(decl.name)) {
        typr.println(i"conflict? $decl $other")
        if (decl.matches(other)) {
          def doubleDefError(decl: Symbol, other: Symbol): Unit = {
            def ofType = if (decl.isType) "" else em": ${other.info}"
            def explanation =
              if (!decl.isRealMethod) ""
              else "\n (the definitions have matching type signatures)"
            ctx.error(em"$decl is already defined as $other$ofType$explanation", decl.pos)
          }
          if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if ((decl is HasDefaultParams) && (other is HasDefaultParams)) {
          ctx.error(em"two or more overloaded variants of $decl have default arguments")
          decl resetFlag HasDefaultParams
        }
      }
      seen(decl.name) = decl :: seen(decl.name)
    }

    cls.info.decls.foreach(checkDecl)
    cls.info match {
      case ClassInfo(_, _, _, _, selfSym: Symbol) => checkDecl(selfSym)
      case _ =>
    }
  }

  def checkParentCall(call: Tree, caller: ClassSymbol)(implicit ctx: Context) =
    if (!ctx.isAfterTyper) {
      val called = call.tpe.classSymbol
      if (caller is Trait)
        ctx.error(i"$caller may not call constructor of $called", call.pos)
      else if (called.is(Trait) && !caller.mixins.contains(called))
        ctx.error(i"""$called is already implemented by super${caller.superClass},
                   |its constructor cannot be called again""", call.pos)
    }

  /** Check that `tpt` does not define a higher-kinded type */
  def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree =
    if (tpt.tpe.isHK && !ctx.compilationUnit.isJava) {
        // be more lenient with missing type params in Java,
        // needed to make pos/java-interop/t1196 work.
      errorTree(tpt, MissingTypeParameterFor(tpt.tpe))
    }
    else tpt

  /** Check that `tpt` does not refer to a singleton type */
  def checkNotSingleton(tpt: Tree, where: String)(implicit ctx: Context): Tree =
    if (tpt.tpe.isInstanceOf[SingletonType]) {
      errorTree(tpt, ex"Singleton type ${tpt.tpe} is not allowed $where")
    }
    else tpt

  /** Verify classes extending AnyVal meet the requirements */
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context) =
    Checking.checkDerivedValueClass(clazz, stats)

  /** Given a parent `parent` of a class `cls`, if `parent` is a trait check that
   *  the superclass of `cls` derived from the superclass of `parent`.
   *
   *  An exception is made if `cls` extends `Any`, and `parent` is `java.io.Serializable`
   *  or `java.lang.Comparable`. These two classes are treated by Scala as universal
   *  traits. E.g. the following is OK:
   *
   *      ... extends Any with java.io.Serializable
   *
   *  The standard library relies on this idiom.
   */
  def checkTraitInheritance(parent: Symbol, cls: ClassSymbol, pos: Position)(implicit ctx: Context): Unit =
    parent match {
      case parent: ClassSymbol if parent is Trait =>
        val psuper = parent.superClass
        val csuper = cls.superClass
        val ok = csuper.derivesFrom(psuper) ||
          parent.is(JavaDefined) && csuper == defn.AnyClass &&
          (parent == defn.JavaSerializableClass || parent == defn.ComparableClass)
        if (!ok)
          ctx.error(em"illegal trait inheritance: super$csuper does not derive from $parent's super$psuper", pos)
      case _ =>
    }

  /** Check that case classes are not inherited by case classes.
   */
  def checkCaseInheritance(parent: Symbol, caseCls: ClassSymbol, pos: Position)(implicit ctx: Context): Unit =
    parent match {
      case parent: ClassSymbol =>
        if (parent is Case)
          ctx.error(ex"""case $caseCls has case ancestor $parent, but case-to-case inheritance is prohibited.
                        |To overcome this limitation, use extractors to pattern match on non-leaf nodes.""", pos)
        else checkCaseInheritance(parent.superClass, caseCls, pos)
      case _ =>
    }

  /** Check that method parameter types do not reference their own parameter
   *  or later parameters in the same parameter section.
   */
  def checkNoForwardDependencies(vparams: List[ValDef])(implicit ctx: Context): Unit = vparams match {
    case vparam :: vparams1 =>
      val check = new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) = tree match {
          case id: Ident if vparams.exists(_.symbol == id.symbol) =>
            ctx.error("illegal forward reference to method parameter", id.pos)
          case _ =>
            traverseChildren(tree)
        }
      }
      check.traverse(vparam.tpt)
      checkNoForwardDependencies(vparams1)
    case Nil =>
  }
}

trait NoChecking extends Checking {
  import tpd._
  override def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(implicit ctx: Context): Type = info
  override def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = tree
  override def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit = ()
  override def checkClassType(tp: Type, pos: Position, traitReq: Boolean, stablePrefixReq: Boolean)(implicit ctx: Context): Type = tp
  override def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = ()
  override def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp
  override def checkInlineConformant(tree: Tree, what: => String)(implicit ctx: Context) = ()
  override def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = ()
  override def checkParentCall(call: Tree, caller: ClassSymbol)(implicit ctx: Context) = ()
  override def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree = tpt
  override def checkNotSingleton(tpt: Tree, where: String)(implicit ctx: Context): Tree = tpt
  override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context) = ()
  override def checkTraitInheritance(parentSym: Symbol, cls: ClassSymbol, pos: Position)(implicit ctx: Context) = ()
  override def checkCaseInheritance(parentSym: Symbol, caseCls: ClassSymbol, pos: Position)(implicit ctx: Context) = ()
  override def checkNoForwardDependencies(vparams: List[ValDef])(implicit ctx: Context): Unit = ()
}
