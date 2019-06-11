package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._
import Types._
import Flags._
import Names._
import StdNames._
import Symbols._
import Trees._
import TreeInfo._
import ProtoTypes._
import Scopes._
import CheckRealizable._
import ErrorReporting.errorTree
import rewrites.Rewrites.patch
import util.Spans.Span

import util.SourcePosition
import util.Spans.Span
import rewrites.Rewrites.patch
import transform.SymUtils._
import transform.ValueClasses._
import Decorators._
import ErrorReporting.{err, errorType}
import config.Printers.{typr, patmatch}
import NameKinds.DefaultGetterName
import NameOps._
import SymDenotations.{NoCompleter, NoDenotation}
import Applications.unapplyArgs
import transform.patmat.SpaceEngine.isIrrefutableUnapply


import collection.mutable
import reporting.diagnostic.Message
import reporting.diagnostic.messages._
import scala.tasty.util.Chars.isOperatorPart

object Checking {
  import tpd._

  /** A general checkBounds method that can be used for TypeApply nodes as
   *  well as for AppliedTypeTree nodes. Also checks that type arguments to
   *  *-type parameters are fully applied.
   *  See TypeOps.boundsViolations for an explanation of the parameters.
   */
  def checkBounds(args: List[tpd.Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type, app: Type = NoType)(implicit ctx: Context): Unit = {
    (args, boundss).zipped.foreach { (arg, bound) =>
      if (!bound.isLambdaSub && !arg.tpe.hasSimpleKind) {
        // see MissingTypeParameterFor
        ctx.error(ex"missing type parameter(s) for $arg", arg.sourcePos)
      }
    }
    for ((arg, which, bound) <- ctx.boundsViolations(args, boundss, instantiate, app))
      ctx.error(
          DoesNotConformToBound(arg.tpe, which, bound)(err),
          arg.sourcePos.focus)
  }

  /** Check that type arguments `args` conform to corresponding bounds in `tl`
   *  Note: This does not check the bounds of AppliedTypeTrees. These
   *  are handled by method checkAppliedType below.
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
  def checkAppliedType(tree: AppliedTypeTree, boundsCheck: Boolean)(implicit ctx: Context): Unit = {
    val AppliedTypeTree(tycon, args) = tree
    // If `args` is a list of named arguments, return corresponding type parameters,
    // otherwise return type parameters unchanged
    val tparams = tycon.tpe.typeParams
    def argNamed(tparam: ParamInfo) = args.find {
      case NamedArg(name, _) => name == tparam.paramName
      case _ => false
    }.getOrElse(TypeTree(tparam.paramRef))
    val orderedArgs = if (hasNamedArg(args)) tparams.map(argNamed) else args
    val bounds = tparams.map(_.paramInfoAsSeenFrom(tree.tpe).bounds)
    def instantiate(bound: Type, args: List[Type]) =
      HKTypeLambda.fromParams(tparams, bound).appliedTo(args)
    if (boundsCheck) checkBounds(orderedArgs, bounds, instantiate, tree.tpe)

    def checkWildcardApply(tp: Type): Unit = tp match {
      case tp @ AppliedType(tycon, _) =>
        if (tycon.isLambdaSub && tp.hasWildcardArg)
          ctx.errorOrMigrationWarning(
            ex"unreducible application of higher-kinded type $tycon to wildcard arguments",
            tree.sourcePos)
      case _ =>
    }
    def checkValidIfApply(implicit ctx: Context): Unit =
      checkWildcardApply(tycon.tpe.appliedTo(args.map(_.tpe)))
    checkValidIfApply(ctx.addMode(Mode.AllowLambdaWildcardApply))
  }

  def checkNoWildcard(tree: Tree)(implicit ctx: Context): Tree = tree.tpe match {
    case tpe: TypeBounds => errorTree(tree, "no wildcard type allowed here")
    case _ => tree
  }

  /** Check that kind of `arg` has the same outline as the kind of paramBounds.
   *  E.g. if `paramBounds` has kind * -> *, `arg` must have that kind as well,
   *  and analogously for all other kinds. This kind checking does not take into account
   *  variances or bounds. The more detailed kind checking is done as part of checkBounds in PostTyper.
   *  The purpose of preCheckKind is to do a rough test earlier in Typer,
   *  in order to prevent scenarios that lead to self application of
   *  types. Self application needs to be avoided since it can lead to stack overflows.
   *  Test cases are neg/i2771.scala and neg/i2771b.scala.
   *  A NoType paramBounds is used as a sign that checking should be suppressed.
   */
  def preCheckKind(arg: Tree, paramBounds: Type)(implicit ctx: Context): Tree =
    if (arg.tpe.widen.isRef(defn.NothingClass) ||
        !paramBounds.exists ||
        arg.tpe.hasSameKindAs(paramBounds.bounds.hi)) arg
    else errorTree(arg, em"Type argument ${arg.tpe} has not the same kind as its bound $paramBounds")

  def preCheckKinds(args: List[Tree], paramBoundss: List[Type])(implicit ctx: Context): List[Tree] = {
    val args1 = args.zipWithConserve(paramBoundss)(preCheckKind)
    args1 ++ args.drop(paramBoundss.length)
      // add any arguments that do not correspond to a parameter back,
      // so the wrong number of parameters is reported afterwards.
  }

  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  def checkInstantiable(tp: Type, posd: Positioned)(implicit ctx: Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.is(AbstractOrTrait))
          ctx.error(CantInstantiateAbstractClassOrTrait(cls, isTrait = cls.is(Trait)), posd.sourcePos)
        if (!cls.is(Module)) {
          // Create a synthetic singleton type instance, and check whether
          // it conforms to the self type of the class as seen from that instance.
          val stp = SkolemType(tp)
          val selfType = cls.asClass.givenSelfType.asSeenFrom(stp, cls)
          if (selfType.exists && !(stp <:< selfType))
            ctx.error(DoesNotConformToSelfTypeCantBeInstantiated(tp, selfType), posd.sourcePos)
        }
      case _ =>
    }

  /** Check that type `tp` is realizable. */
  def checkRealizable(tp: Type, posd: Positioned, what: String = "path")(implicit ctx: Context): Unit = {
    val rstatus = realizability(tp)
    if (rstatus ne Realizable)
      ctx.errorOrMigrationWarning(em"$tp is not a legal $what\nsince it${rstatus.msg}", posd.sourcePos)
  }

  /** A type map which checks that the only cycles in a type are F-bounds
   *  and that protects all F-bounded references by LazyRefs.
   */
  class CheckNonCyclicMap(sym: Symbol, reportErrors: Boolean)(implicit ctx: Context) extends TypeMap {

    /** Set of type references whose info is currently checked */
    private val locked = mutable.Set[TypeRef]()

    /** Are cycles allowed within nested refinedInfos of currently checked type? */
    private[this] var nestedCycleOK = false

    /** Are cycles allowed within currently checked type? */
    private[this] var cycleOK = false

    /** A diagnostic output string that indicates the position of the last
     *  part of a type bounds checked by checkInfo. Possible choices:
     *  alias, lower bound, upper bound.
     */
    var where: String = ""

    /** The last type top-level type checked when a CyclicReference occurs. */
    var lastChecked: Type = NoType

    private def checkPart(tp: Type, w: String) =
      try apply(tp)
      finally {
        where = w
        lastChecked = tp
      }

    private def checkUpper(tp: Type, w: String) = {
      val saved = nestedCycleOK
      nestedCycleOK = true
      try checkPart(tp, w)
      finally nestedCycleOK = saved
    }

    /** Check info `tp` for cycles. Throw CyclicReference for illegal cycles,
     *  break direct cycle with a LazyRef for legal, F-bounded cycles.
     */
    def checkInfo(tp: Type): Type = tp match {
      case tp @ TypeAlias(alias) =>
        tp.derivedAlias(checkPart(alias, "alias"))
      case tp @ MatchAlias(alias) =>
        tp.derivedAlias(checkUpper(alias, "match"))
      case tp @ TypeBounds(lo, hi) =>
        tp.derivedTypeBounds(checkPart(lo, "lower bound"), checkUpper(hi, "upper bound"))
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
      case tp @ AppliedType(tycon, args) =>
        tp.derivedAppliedType(this(tycon), args.mapConserve(this(_, nestedCycleOK, nestedCycleOK)))
      case tp @ RefinedType(parent, name, rinfo) =>
        tp.derivedRefinedType(this(parent), name, this(rinfo, nestedCycleOK, nestedCycleOK))
      case tp: RecType =>
        tp.rebind(this(tp.parent))
      case tp @ TypeRef(pre, _) =>
        try {
          // A prefix is interesting if it might contain (transitively) a reference
          // to symbol `sym` itself. We only check references with interesting
          // prefixes for cycles. This pruning is done in order not to force
          // global symbols when doing the cyclicity check.
          def isInteresting(prefix: Type): Boolean = prefix.stripTypeVar match {
            case NoPrefix => true
            case prefix: ThisType =>
              sym.owner.isClass && (
                prefix.cls.isContainedIn(sym.owner)    // sym reachable through outer references
                || sym.owner.isContainedIn(prefix.cls) // sym reachable through member references
              )
            case prefix: NamedType =>
              (!sym.is(Private) && prefix.derivesFrom(sym.owner)) ||
              (!prefix.symbol.moduleClass.isStaticOwner && isInteresting(prefix.prefix))
            case SuperType(thistp, _) => isInteresting(thistp)
            case AndType(tp1, tp2) => isInteresting(tp1) || isInteresting(tp2)
            case OrType(tp1, tp2) => isInteresting(tp1) && isInteresting(tp2)
            case _: RefinedOrRecType | _: AppliedType => true
            case _ => false
          }

          if (isInteresting(pre)) {
            val pre1 = this(pre, false, false)
            if (locked.contains(tp) || tp.symbol.infoOrCompleter.isInstanceOf[NoCompleter])
              throw CyclicReference(tp.symbol)
            locked += tp
            try checkInfo(tp.info)
            finally locked -= tp
            tp.withPrefix(pre1)
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

  /** If `sym` has an operator name, check that it has an @alpha annotation under -strict */
  def checkValidOperator(sym: Symbol)(implicit ctx: Context): Unit =
    sym.name.toTermName match {
      case name: SimpleName
      if name.exists(isOperatorPart) &&
        !sym.getAnnotation(defn.AlphaAnnot).isDefined &&
        !sym.is(Synthetic) &&
        !name.isConstructorName &&
        ctx.settings.strict.value =>
        ctx.deprecationWarning(
          i"$sym has an operator name; it should come with an @alpha annotation", sym.sourcePos)
      case _ =>
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
          errorType(i"illegal cyclic reference: ${checker.where} ${checker.lastChecked} of $sym refers back to the type itself", sym.sourcePos)
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
      ctx.warning(i"$what reference in refinement is deprecated", tree.sourcePos)
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

  /** Check type members inherited from different `parents` of `joint` type for cycles,
   *  unless a type with the same name aleadry appears in `decls`.
   *  @return    true iff no cycles were detected
   */
  def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, posd: Positioned)(implicit ctx: Context): Unit = {
    def qualifies(sym: Symbol) = sym.name.isTypeName && !sym.is(Private)
    val abstractTypeNames =
      for (parent <- parents; mbr <- parent.abstractTypeMembers if qualifies(mbr.symbol))
      yield mbr.name.asTypeName

   for (name <- abstractTypeNames)
      try {
        val mbr = joint.member(name)
        mbr.info match {
          case bounds: TypeBounds =>
            !checkNonCyclic(mbr.symbol, bounds, reportErrors = true).isError
          case _ =>
            true
        }
      }
      catch {
        case ex: RecursionOverflow =>
          ctx.error(em"cyclic reference involving type $name", posd.sourcePos)
          false
      }
  }

  /** Check that symbol's definition is well-formed. */
  def checkWellFormed(sym: Symbol)(implicit ctx: Context): Unit = {
    def fail(msg: Message) = ctx.error(msg, sym.sourcePos)

    def checkWithDeferred(flag: FlagSet) =
      if (sym.is(flag))
        fail(AbstractMemberMayNotHaveModifier(sym, flag))
    def checkNoConflict(flag1: FlagSet, flag2: FlagSet, msg: => String) =
      if (sym.is(allOf(flag1, flag2))) fail(msg)
    def checkCombination(flag1: FlagSet, flag2: FlagSet) =
      checkNoConflict(flag1, flag2, i"illegal combination of modifiers: `$flag1` and `$flag2` for: $sym")
    def checkApplicable(flag: FlagSet, ok: Boolean) =
      if (!ok && !sym.is(Synthetic))
        fail(i"modifier `$flag` is not allowed for this definition")

    if (sym.is(Inline) &&
          (  sym.is(ParamAccessor) && sym.owner.isClass
          || sym.is(TermParam) && !sym.owner.isInlineMethod
          ))
      fail(ParamsNoInline(sym.owner))

    if (sym.is(ImplicitOrImplied)) {
      if (sym.owner.is(Package))
        fail(TopLevelCantBeImplicit(sym))
      if (sym.isType)
        fail(TypesAndTraitsCantBeImplicit())
    }
    if (!sym.isClass && sym.is(Abstract))
      fail(OnlyClassesCanBeAbstract(sym))
    if (sym.is(AbsOverride) && !sym.owner.is(Trait))
      fail(AbstractOverrideOnlyInTraits(sym))
    if (sym.is(Trait) && sym.is(Final))
      fail(TraitsMayNotBeFinal(sym))
    // Skip ModuleVal since the annotation will also be on the ModuleClass
    if (sym.hasAnnotation(defn.TailrecAnnot) && !sym.is(Method | ModuleVal))
      fail(TailrecNotApplicable(sym))
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
    checkCombination(Final, Sealed)
    checkCombination(Private, Protected)
    checkCombination(Abstract, Override)
    checkCombination(Private, Override)
    checkCombination(Lazy, Inline)
    checkNoConflict(Lazy, ParamAccessor, s"parameter may not be `lazy`")
    if (sym.is(Inline)) checkApplicable(Inline, sym.isTerm && !sym.is(Mutable | Module))
    if (sym.is(Lazy)) checkApplicable(Lazy, !sym.is(Method | Mutable))
    if (sym.isType && !sym.is(Deferred))
      for (cls <- sym.allOverriddenSymbols.filter(_.isClass)) {
        fail(CannotHaveSameNameAs(sym, cls, CannotHaveSameNameAs.CannotBeOverridden))
        sym.setFlag(Private) // break the overriding relationship by making sym Private
      }
    if (sym.is(Erased))
      checkApplicable(Erased, !sym.is(MutableOrLazy))
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
  def checkNoPrivateLeaks(sym: Symbol, pos: SourcePosition)(implicit ctx: Context): Type = {
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
        other.is(Private, butNot = TypeParam) && {
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
                (() => em"non-private $sym refers to private ${tp.symbol}\nin its type signature ${sym.info}") :: errors
              tp
            }
            else mapOver(tp)
          if ((errors ne prevErrors) && !sym.isType && tp.info.isTypeAlias) {
            // try to dealias to avoid a leak error
            val savedErrors = errors
            errors = prevErrors
            val tp2 = apply(tp.superType)
            if (errors eq prevErrors) tp1 = tp2
            else errors = savedErrors
          }
          tp1
        case tp: ClassInfo =>
          def transformedParent(tp: Type): Type = tp match {
            case ref: TypeRef => ref
            case ref: AppliedType => ref
            case AnnotatedType(parent, annot) =>
              AnnotatedType(transformedParent(parent), annot)
            case _ => defn.ObjectType // can happen if class files are missing
          }
          tp.derivedClassInfo(
            prefix = apply(tp.prefix),
            classParents =
              tp.parents.map(p => transformedParent(apply(p)))
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
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context): Unit = {
    def checkValueClassMember(stat: Tree) = stat match {
      case _: TypeDef if stat.symbol.isClass =>
        ctx.error(ValueClassesMayNotDefineInner(clazz, stat.symbol), stat.sourcePos)
      case _: ValDef if !stat.symbol.is(ParamAccessor) =>
        ctx.error(ValueClassesMayNotDefineNonParameterField(clazz, stat.symbol), stat.sourcePos)
      case _: DefDef if stat.symbol.isConstructor =>
        ctx.error(ValueClassesMayNotDefineASecondaryConstructor(clazz, stat.symbol), stat.sourcePos)
      case _: MemberDef | _: Import | EmptyTree =>
      // ok
      case _ =>
        ctx.error(ValueClassesMayNotContainInitalization(clazz), stat.sourcePos)
    }
    if (isDerivedValueClass(clazz)) {
      if (clazz.is(Trait))
        ctx.error(CannotExtendAnyVal(clazz), clazz.sourcePos)
      if (clazz.is(Abstract))
        ctx.error(ValueClassesMayNotBeAbstract(clazz), clazz.sourcePos)
      if (!clazz.isStatic)
        ctx.error(ValueClassesMayNotBeContainted(clazz), clazz.sourcePos)
      if (isCyclic(clazz.asClass))
        ctx.error(ValueClassesMayNotWrapItself(clazz), clazz.sourcePos)
      else {
        val clParamAccessors = clazz.asClass.paramAccessors.filter { param =>
          param.isTerm && !param.is(Flags.Accessor)
        }
        clParamAccessors match {
          case param :: params =>
            if (param.is(Mutable))
              ctx.error(ValueClassParameterMayNotBeAVar(clazz, param), param.sourcePos)
            if (param.info.isInstanceOf[ExprType])
              ctx.error(ValueClassParameterMayNotBeCallByName(clazz, param), param.sourcePos)
            if (param.is(Erased))
              ctx.error("value class first parameter cannot be `erased`", param.sourcePos)
            else {
              for (p <- params if !p.is(Erased))
                ctx.error("value class can only have one non `erased` parameter", p.sourcePos)
            }
          case Nil =>
            ctx.error(ValueClassNeedsOneValParam(clazz), clazz.sourcePos)
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

  def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, posd: Positioned)(implicit ctx: Context): Unit =
    Checking.checkNonCyclicInherited(joint, parents, decls, posd)

  /** Check that Java statics and packages can only be used in selections.
   */
  def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = {
    if (!proto.isInstanceOf[SelectionProto] && !proto.isInstanceOf[ApplyingProto]) {
      val sym = tree.tpe.termSymbol
      // The check is avoided inside Java compilation units because it always fails
      // on the singleton type Module.type.
      if ((sym is Package) || ((sym is JavaModule) && !ctx.compilationUnit.isJava)) ctx.error(JavaSymbolIsNotAValue(sym), tree.sourcePos)
    }
    tree
  }

  /** Check that type `tp` is stable. */
  def checkStable(tp: Type, pos: SourcePosition)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(ex"$tp is not stable", pos)

  /** Check that all type members of `tp` have realizable bounds */
  def checkRealizableBounds(cls: Symbol, pos: SourcePosition)(implicit ctx: Context): Unit = {
    val rstatus = boundsRealizability(cls.thisType)
    if (rstatus ne Realizable)
      ctx.error(ex"$cls cannot be instantiated since it${rstatus.msg}", pos)
  }

  /** Check that pattern `pat` is irrefutable for scrutinee tye `pt`.
   *  This means `pat` is either marked @unchecked or `pt` conforms to the
   *  pattern's type. If pattern is an UnApply, do the check recursively.
   */
  def checkIrrefutable(pat: Tree, pt: Type, isPatDef: Boolean)(implicit ctx: Context): Boolean = {

    def fail(pat: Tree, pt: Type): Boolean = {
      var reportedPt = pt.dropAnnot(defn.UncheckedAnnot)
      if (!pat.tpe.isSingleton) reportedPt = reportedPt.widen
      val problem = if (pat.tpe <:< reportedPt) "is more specialized than" else "does not match"
      val fix = if (isPatDef) "`: @unchecked` after" else "`case ` before"
      ctx.errorOrMigrationWarning(
        ex"""pattern's type ${pat.tpe} $problem the right hand side expression's type $reportedPt
            |
            |If the narrowing is intentional, this can be communicated by writing $fix the full pattern.${err.rewriteNotice}""",
        pat.sourcePos)
      false
    }

    def check(pat: Tree, pt: Type): Boolean = (pt <:< pat.tpe) || fail(pat, pt)

    def recur(pat: Tree, pt: Type): Boolean =
      !ctx.settings.strict.value || // only in -strict mode for now since mitigations work only after this PR
      pat.tpe.widen.hasAnnotation(defn.UncheckedAnnot) || {
        patmatch.println(i"check irrefutable $pat: ${pat.tpe} against $pt")
        pat match {
          case Bind(_, pat1) =>
            recur(pat1, pt)
          case UnApply(fn, _, pats) =>
            check(pat, pt) &&
            (isIrrefutableUnapply(fn) || fail(pat, pt)) && {
              val argPts = unapplyArgs(fn.tpe.widen.finalResultType, fn, pats, pat.sourcePos)
              pats.corresponds(argPts)(recur)
            }
          case Alternative(pats) =>
            pats.forall(recur(_, pt))
          case Typed(arg, tpt) =>
            check(pat, pt) && recur(arg, pt)
          case Ident(nme.WILDCARD) =>
            true
          case _ =>
            check(pat, pt)
        }
      }

    recur(pat, pt)
  }

  /** Check that `path` is a legal prefix for an import or export clause */
  def checkLegalImportPath(path: Tree)(implicit ctx: Context): Unit = {
    checkStable(path.tpe, path.sourcePos)
    if (!ctx.isAfterTyper) Checking.checkRealizable(path.tpe, path.posd)
  }

 /**  Check that `tp` is a class type.
  *   Also, if `traitReq` is true, check that `tp` is a trait.
  *   Also, if `stablePrefixReq` is true and phase is not after RefChecks,
  *   check that class prefix is stable.
   *  @return  `tp` itself if it is a class or trait ref, ObjectType if not.
   */
  def checkClassType(tp: Type, pos: SourcePosition, traitReq: Boolean, stablePrefixReq: Boolean)(implicit ctx: Context): Type =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        if (traitReq && !(tref.symbol is Trait)) ctx.error(TraitIsExpected(tref.symbol), pos)
        if (stablePrefixReq && ctx.phase <= ctx.refchecksPhase) checkStable(tref.prefix, pos)
        tp
      case _ =>
        ctx.error(ex"$tp is not a class type", pos)
        defn.ObjectType
    }

  /** If `sym` is an implicit conversion, check that implicit conversions are enabled.
   *  @pre  sym.is(ImplicitOrGiven)
   */
  def checkImplicitConversionDefOK(sym: Symbol)(implicit ctx: Context): Unit = {
    def check(): Unit = {
      checkFeature(
        nme.implicitConversions,
        i"Definition of implicit conversion $sym",
        ctx.owner.topLevelClass,
        sym.sourcePos)
    }

    sym.info.stripPoly match {
      case mt @ MethodType(_ :: Nil)
      if !mt.isImplicitMethod && !sym.is(Synthetic) => // it's a conversion
        check()
      case AppliedType(tycon, _)
      if tycon.derivesFrom(defn.ConversionClass) && !sym.is(Synthetic) =>
        check()
      case _ =>
    }
  }

  /** If `sym` is an implicit conversion, check that that implicit conversions are enabled, unless
   *    - it is synthetic
   *    - it is has the same owner as one of the classes it converts to (modulo companions)
   *    - it is defined in Predef
   *    - it is the scala.reflect.Selectable.reflectiveSelectable conversion
   */
  def checkImplicitConversionUseOK(sym: Symbol, posd: Positioned)(implicit ctx: Context): Unit =
    if (sym.exists) {
      val conv =
        if (sym.is(ImplicitOrGiven)) sym
        else {
          assert(sym.name == nme.apply)
          sym.owner
        }
      val conversionOK =
        conv.is(Synthetic) ||
        sym.info.finalResultType.classSymbols.exists(_.isLinkedWith(conv.owner)) ||
        defn.isPredefClass(conv.owner) ||
        conv.name == nme.reflectiveSelectable && conv.maybeOwner.maybeOwner.maybeOwner == defn.ScalaPackageClass
      if (!conversionOK)
        checkFeature(nme.implicitConversions,
          i"Use of implicit conversion ${conv.showLocated}", NoSymbol, posd.sourcePos)
    }

  private def infixOKSinceFollowedBy(tree: untpd.Tree): Boolean = tree match {
    case _: untpd.Block | _: untpd.Match => true
    case _ => false
  }

  /** Check that `tree` is a valid infix operation. That is, if the
   *  operator is alphanumeric, it must be declared `@infix`.
   */
  def checkValidInfix(tree: untpd.InfixOp, meth: Symbol)(implicit ctx: Context): Unit = {

    def isInfix(sym: Symbol): Boolean =
      sym.hasAnnotation(defn.InfixAnnot) ||
      defn.isInfix(sym) ||
      sym.name.isUnapplyName &&
        sym.owner.is(Module) && sym.owner.linkedClass.is(Case) &&
        isInfix(sym.owner.linkedClass)

    tree.op match {
      case _: untpd.BackquotedIdent =>
        ()
      case Ident(name: Name) =>
        name.toTermName match {
          case name: SimpleName
          if !name.exists(isOperatorPart) &&
            !isInfix(meth) &&
            !meth.maybeOwner.is(Scala2x) &&
            !infixOKSinceFollowedBy(tree.right) &&
            ctx.settings.strict.value =>
            val (kind, alternative) =
              if (ctx.mode.is(Mode.Type))
                ("type", (n: Name) => s"prefix syntax $n[...]")
              else if (ctx.mode.is(Mode.Pattern))
                ("extractor", (n: Name) => s"prefix syntax $n(...)")
              else
                ("method", (n: Name) => s"method syntax .$n(...)")
            ctx.deprecationWarning(
              i"""Alphanumeric $kind $name is not declared @infix; it should not be used as infix operator.
                 |The operation can be rewritten automatically to `$name` under -deprecation -rewrite.
                 |Or rewrite to ${alternative(name)} manually.""",
              tree.op.sourcePos)
            if (ctx.settings.deprecation.value) {
              patch(Span(tree.op.span.start, tree.op.span.start), "`")
              patch(Span(tree.op.span.end, tree.op.span.end), "`")
            }
          case _ =>
        }
    }
  }

  /** Issue a feature warning if feature is not enabled */
  def checkFeature(name: TermName,
                   description: => String,
                   featureUseSite: Symbol,
                   pos: SourcePosition)(implicit ctx: Context): Unit =
    if (!ctx.featureEnabled(name))
      ctx.featureWarning(name.toString, description, featureUseSite, required = false, pos)

  /** Check that `tp` is a class type and that any top-level type arguments in this type
   *  are feasible, i.e. that their lower bound conforms to their upper bound. If a type
   *  argument is infeasible, issue and error and continue with upper bound.
   */
  def checkFeasibleParent(tp: Type, pos: SourcePosition, where: => String = "")(implicit ctx: Context): Type = {
    def checkGoodBounds(tp: Type) = tp match {
      case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
        ctx.error(ex"no type exists between low bound $lo and high bound $hi$where", pos)
        TypeBounds(hi, hi)
      case _ =>
        tp
    }
    tp match {
      case tp @ AndType(tp1, tp2) =>
        ctx.error(s"conflicting type arguments$where", pos)
        tp1
      case tp @ AppliedType(tycon, args) =>
        tp.derivedAppliedType(tycon, args.mapConserve(checkGoodBounds))
      case tp: RefinedType =>
        tp.derivedRefinedType(tp.parent, tp.refinedName, checkGoodBounds(tp.refinedInfo))
      case _ =>
        tp
    }
  }

  /** Check that `tree` can be right hand-side or argument to `inline` value or parameter. */
  def checkInlineConformant(tree: Tree, isFinal: Boolean, what: => String)(implicit ctx: Context): Unit = {
    // final vals can be marked inline even if they're not pure, see Typer#patchFinalVals
    val purityLevel = if (isFinal) Idempotent else Pure
    tree.tpe.widenTermRefExpr match {
      case tp: ConstantType if exprPurity(tree) >= purityLevel => // ok
      case _ =>
        tree match {
          case Typed(expr, _) =>
            checkInlineConformant(expr, isFinal, what)
          case Inlined(_, Nil, expr) =>
            checkInlineConformant(expr, isFinal, what)
          case SeqLiteral(elems, _) =>
            elems.foreach(elem => checkInlineConformant(elem, isFinal, what))
          case Apply(fn, List(arg)) if defn.WrapArrayMethods().contains(fn.symbol) =>
            checkInlineConformant(arg, isFinal, what)
          case _ =>
            def isCaseClassApply(sym: Symbol): Boolean =
              sym.name == nme.apply && sym.is(Synthetic) && sym.owner.is(Module) && sym.owner.companionClass.is(Case)
            def isCaseClassNew(sym: Symbol): Boolean =
              sym.isPrimaryConstructor && sym.owner.is(Case) && sym.owner.isStatic
            def isCaseObject(sym: Symbol): Boolean = {
              // TODO add alias to Nil in scala package
              sym.is(Case) && sym.is(Module)
            }
            val allow =
              ctx.erasedTypes ||
              ctx.inInlineMethod ||
              (tree.symbol.isStatic && isCaseObject(tree.symbol) || isCaseClassApply(tree.symbol)) ||
              isCaseClassNew(tree.symbol)

            if (!allow) ctx.error(em"$what must be a known value", tree.sourcePos)
            else {
              def checkArgs(tree: Tree): Unit = tree match {
                case Apply(fn, args) =>
                  args.foreach(arg => checkInlineConformant(arg, isFinal, what))
                  checkArgs(fn)
                case _ =>
              }
              checkArgs(tree)
            }
        }
    }
  }

  /** A hook to exclude selected symbols from double declaration check */
  def excludeFromDoubleDeclCheck(sym: Symbol)(implicit ctx: Context): Boolean = false

  /** Check that class does not declare same symbol twice */
  def checkNoDoubleDeclaration(cls: Symbol)(implicit ctx: Context): Unit = {
    val seen = new mutable.HashMap[Name, List[Symbol]] {
      override def default(key: Name) = Nil
    }
    typr.println(i"check no double declarations $cls")

    def checkDecl(decl: Symbol): Unit = {
      for (other <- seen(decl.name) if (!decl.isAbsent && !other.isAbsent)) {
        typr.println(i"conflict? $decl $other")
        def javaFieldMethodPair =
          decl.is(JavaDefined) && other.is(JavaDefined) &&
          decl.is(Method) != other.is(Method)
        if (decl.matches(other) && !javaFieldMethodPair) {
          def doubleDefError(decl: Symbol, other: Symbol): Unit =
            if (!decl.info.isErroneous && !other.info.isErroneous)
              ctx.error(DoubleDefinition(decl, other, cls), decl.sourcePos)
          if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if ((decl is HasDefaultParams) && (other is HasDefaultParams)) {
          ctx.error(em"two or more overloaded variants of $decl have default arguments", decl.sourcePos)
          decl resetFlag HasDefaultParams
        }
      }
      if (!excludeFromDoubleDeclCheck(decl))
        seen(decl.name) = decl :: seen(decl.name)
    }

    cls.info.decls.foreach(checkDecl)
    cls.info match {
      case ClassInfo(_, _, _, _, selfSym: Symbol) => checkDecl(selfSym)
      case _ =>
    }
  }

  def checkParentCall(call: Tree, caller: ClassSymbol)(implicit ctx: Context): Unit =
    if (!ctx.isAfterTyper) {
      val called = call.tpe.classSymbol
      if (caller is Trait)
        ctx.error(i"$caller may not call constructor of $called", call.sourcePos)
      else if (called.is(Trait) && !caller.mixins.contains(called))
        ctx.error(i"""$called is already implemented by super${caller.superClass},
                   |its constructor cannot be called again""", call.sourcePos)

      if (caller.is(Module)) {
        val traverser = new TreeTraverser {
          def traverse(tree: Tree)(implicit ctx: Context) = tree match {
            case tree: RefTree if tree.isTerm && (tree.tpe.widen.classSymbol eq caller) =>
              ctx.error("super constructor cannot be passed a self reference", tree.sourcePos)
            case _ =>
              traverseChildren(tree)
          }
        }
        traverser.traverse(call)
      }

      // Check that constructor call is of the form _.<init>(args1)...(argsN).
      // This guards against calls resulting from inserted implicits or applies.
      def checkLegalConstructorCall(tree: Tree, encl: Tree, kind: String): Unit = tree match {
        case Apply(fn, _) => checkLegalConstructorCall(fn, tree, "")
        case TypeApply(fn, _) => checkLegalConstructorCall(fn, tree, "type ")
        case Select(_, nme.CONSTRUCTOR) => // ok
        case _ => ctx.error(s"too many ${kind}arguments in parent constructor", encl.sourcePos)
      }
      call match {
        case Apply(fn, _) => checkLegalConstructorCall(fn, call, "")
        case _ =>
      }
    }

  /** Check that `tpt` does not define a higher-kinded type */
  def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree =
    if (!tpt.tpe.hasSimpleKind && !ctx.compilationUnit.isJava) {
        // be more lenient with missing type params in Java,
        // needed to make pos/java-interop/t1196 work.
      errorTree(tpt, MissingTypeParameterFor(tpt.tpe))
    }
    else tpt

  /** Verify classes extending AnyVal meet the requirements */
  def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context): Unit =
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
  def checkTraitInheritance(parent: Symbol, cls: ClassSymbol, pos: SourcePosition)(implicit ctx: Context): Unit =
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
  def checkCaseInheritance(parent: Symbol, caseCls: ClassSymbol, pos: SourcePosition)(implicit ctx: Context): Unit =
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
            ctx.error("illegal forward reference to method parameter", id.sourcePos)
          case _ =>
            traverseChildren(tree)
        }
      }
      check.traverse(vparam.tpt)
      checkNoForwardDependencies(vparams1)
    case Nil =>
  }

  /** Check that all named types that form part of this type have a denotation.
   *  Called on inferred (result) types of ValDefs and DefDefs.
   *  This could fail for types where the member was originally available as part
   *  of the self type, yet is no longer visible once the `this` has been replaced
   *  by some other prefix. See neg/i3083.scala
   */
  def checkMembersOK(tp: Type, pos: SourcePosition)(implicit ctx: Context): Type = {
    var ok = true
    val check: Type => Unit = {
      case ref: NamedType =>
        val d = try ref.denot catch { case ex: TypeError => NoDenotation }
        if (!d.exists) {
          ctx.error(em"$ref is not defined in inferred type $tp", pos)
          ok = false
        }
      case _ =>
    }
    tp.foreachPart(check, stopAtStatic = true)
    if (ok) tp else UnspecifiedErrorType
  }

  /** Check that all non-synthetic references of the form `<ident>` or
   *  `this.<ident>` in `tree` that refer to a member of `badOwner` are
   *  `allowed`. Also check that there are no other explicit `this` references
   *  to `badOwner`.
   */
  def checkRefsLegal(tree: tpd.Tree, badOwner: Symbol, allowed: (Name, Symbol) => Boolean, where: String)(implicit ctx: Context): Unit = {
    val checker = new TreeTraverser {
      def traverse(t: Tree)(implicit ctx: Context) = {
        def check(owner: Symbol, checkedSym: Symbol) =
          if (t.span.isSourceDerived && owner == badOwner)
            t match {
              case t: RefTree if allowed(t.name, checkedSym) =>
              case _ => ctx.error(i"illegal reference to $checkedSym from $where", t.sourcePos)
            }
        val sym = t.symbol
        t match {
          case Ident(_) | Select(This(_), _) => check(sym.maybeOwner, sym)
          case This(_) => check(sym, sym)
          case _ => traverseChildren(t)
        }
      }
    }
    checker.traverse(tree)
  }

  /** Check that we are in an inline context (inside an inline method or in inline code) */
  def checkInInlineContext(what: String, posd: Positioned)(implicit ctx: Context): Unit =
    if (!ctx.inInlineMethod && !ctx.isInlineContext) {
      val inInlineUnapply = ctx.owner.ownersIterator.exists(owner =>
        owner.name.isUnapplyName && owner.is(Inline) && owner.is(Method))
      val msg =
        if (inInlineUnapply) "cannot be used in an inline unapply"
        else "can only be used in an inline method"
      ctx.error(em"$what $msg", posd.sourcePos)
    }

  /** 1. Check that all case classes that extend `scala.Enum` are `enum` cases
   *  2. Check that case class `enum` cases do not extend java.lang.Enum.
   */
  def checkEnum(cdef: untpd.TypeDef, cls: Symbol, firstParent: Symbol)(implicit ctx: Context): Unit = {
    import untpd.modsDeco
    def isEnumAnonCls =
      cls.isAnonymousClass &&
      cls.owner.isTerm &&
      (cls.owner.flagsUNSAFE.is(Case) || cls.owner.name == nme.DOLLAR_NEW)
    if (!isEnumAnonCls) {
      if (cdef.mods.isEnumCase) {
        if (cls.derivesFrom(defn.JavaEnumClass))
          ctx.error(em"paramerized case is not allowed in an enum that extends java.lang.Enum", cdef.sourcePos)
      }
      else if (cls.is(Case) || firstParent.is(Enum))
        // Since enums are classes and Namer checks that classes don't extend multiple classes, we only check the class
        // parent.
        //
        // Unlike firstParent.derivesFrom(defn.EnumClass), this test allows inheriting from `Enum` by hand;
        // see enum-List-control.scala.
        ctx.error(ClassCannotExtendEnum(cls, firstParent), cdef.sourcePos)
    }
  }

  /** Check that all references coming from enum cases in an enum companion object
   *  are legal.
   *  @param  cdef     the enum companion object class
   *  @param  enumCtx  the context immediately enclosing the corresponding enum
   */
  def checkEnumCaseRefsLegal(cdef: TypeDef, enumCtx: Context)(implicit ctx: Context): Unit = {

    def checkCaseOrDefault(stat: Tree, caseCtx: Context) = {

      def check(tree: Tree) = {
        // allow access to `sym` if a typedIdent just outside the enclosing enum
        // would have produced the same symbol without errors
        def allowAccess(name: Name, sym: Symbol): Boolean = {
          val testCtx = caseCtx.fresh.setNewTyperState()
          val ref = ctx.typer.typedIdent(untpd.Ident(name), WildcardType)(testCtx)
          ref.symbol == sym && !testCtx.reporter.hasErrors
        }
        checkRefsLegal(tree, cdef.symbol, allowAccess, "enum case")
      }

      if (stat.symbol.is(Case))
        stat match {
          case TypeDef(_, Template(DefDef(_, tparams, vparamss, _, _), parents, _, _)) =>
            tparams.foreach(check)
            vparamss.foreach(_.foreach(check))
            parents.foreach(check)
          case vdef: ValDef =>
            vdef.rhs match {
              case Block((clsDef @ TypeDef(_, impl: Template)) :: Nil, _)
              if clsDef.symbol.isAnonymousClass =>
                impl.parents.foreach(check)
              case _ =>
            }
          case _ =>
        }
      else if (stat.symbol.is(Module) && stat.symbol.linkedClass.is(Case))
        stat match {
          case TypeDef(_, impl: Template) =>
            for ((defaultGetter @
                  DefDef(DefaultGetterName(nme.CONSTRUCTOR, _), _, _, _, _)) <- impl.body)
              check(defaultGetter.rhs)
          case _ =>
        }
    }

    cdef.rhs match {
      case impl: Template =>
        def isCase(stat: Tree) = stat match {
          case _: ValDef | _: TypeDef => stat.symbol.is(Case)
          case _ => false
        }
        val cases =
          for (stat <- impl.body if isCase(stat))
          yield untpd.Ident(stat.symbol.name.toTermName)
        val caseImport: Import = Import(importImplied = false, ref(cdef.symbol), cases)
        val caseCtx = enumCtx.importContext(caseImport, caseImport.symbol)
        for (stat <- impl.body) checkCaseOrDefault(stat, caseCtx)
      case _ =>
    }
  }

  /** Check that symbol's external name does not clash with symbols defined in the same scope */
  def checkNoAlphaConflict(stats: List[Tree])(implicit ctx: Context): Unit = {
    var seen = Set[Name]()
    for (stat <- stats) {
      val sym = stat.symbol
      val ename = sym.erasedName
      if (ename != sym.name) {
        val preExisting = ctx.effectiveScope.lookup(ename)
        if (preExisting.exists || seen.contains(ename))
          ctx.error(em"@alpha annotation ${'"'}$ename${'"'} clashes with other definition is same scope", stat.sourcePos)
        seen += ename
      }
    }
  }
}

trait ReChecking extends Checking {
  import tpd._
  override def checkEnum(cdef: untpd.TypeDef, cls: Symbol, firstParent: Symbol)(implicit ctx: Context): Unit = ()
  override def checkRefsLegal(tree: tpd.Tree, badOwner: Symbol, allowed: (Name, Symbol) => Boolean, where: String)(implicit ctx: Context): Unit = ()
  override def checkEnumCaseRefsLegal(cdef: TypeDef, enumCtx: Context)(implicit ctx: Context): Unit = ()
}

trait NoChecking extends ReChecking {
  import tpd._
  override def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(implicit ctx: Context): Type = info
  override def checkNonCyclicInherited(joint: Type, parents: List[Type], decls: Scope, posd: Positioned)(implicit ctx: Context): Unit = ()
  override def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = tree
  override def checkStable(tp: Type, pos: SourcePosition)(implicit ctx: Context): Unit = ()
  override def checkClassType(tp: Type, pos: SourcePosition, traitReq: Boolean, stablePrefixReq: Boolean)(implicit ctx: Context): Type = tp
  override def checkImplicitConversionDefOK(sym: Symbol)(implicit ctx: Context): Unit = ()
  override def checkImplicitConversionUseOK(sym: Symbol, posd: Positioned)(implicit ctx: Context): Unit = ()
  override def checkFeasibleParent(tp: Type, pos: SourcePosition, where: => String = "")(implicit ctx: Context): Type = tp
  override def checkInlineConformant(tree: Tree, isFinal: Boolean, what: => String)(implicit ctx: Context): Unit = ()
  override def checkNoAlphaConflict(stats: List[Tree])(implicit ctx: Context): Unit = ()
  override def checkParentCall(call: Tree, caller: ClassSymbol)(implicit ctx: Context): Unit = ()
  override def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree = tpt
  override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(implicit ctx: Context): Unit = ()
  override def checkTraitInheritance(parentSym: Symbol, cls: ClassSymbol, pos: SourcePosition)(implicit ctx: Context): Unit = ()
  override def checkCaseInheritance(parentSym: Symbol, caseCls: ClassSymbol, pos: SourcePosition)(implicit ctx: Context): Unit = ()
  override def checkNoForwardDependencies(vparams: List[ValDef])(implicit ctx: Context): Unit = ()
  override def checkMembersOK(tp: Type, pos: SourcePosition)(implicit ctx: Context): Type = tp
  override def checkInInlineContext(what: String, posd: Positioned)(implicit ctx: Context): Unit = ()
  override def checkValidInfix(tree: untpd.InfixOp, meth: Symbol)(implicit ctx: Context): Unit = ()
  override def checkFeature(name: TermName, description: => String, featureUseSite: Symbol, pos: SourcePosition)(implicit ctx: Context): Unit = ()
}
