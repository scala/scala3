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
import util.{Stats, SimpleMap}
import util.common._
import transform.SymUtils._
import Decorators._
import Uniques._
import ErrorReporting.{err, errorType, DiagnosticString}
import config.Printers._
import collection.mutable
import SymDenotations.NoCompleter

object Checking {
  import tpd._

  /** A general checkBounds method that can be used for TypeApply nodes as
   *  well as for AppliedTypeTree nodes.
   */
  def checkBounds(args: List[tpd.Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type)(implicit ctx: Context) =
    for ((arg, which, bound) <- ctx.boundsViolations(args, boundss, instantiate))
      ctx.error(
          d"Type argument ${arg.tpe} does not conform to $which bound $bound ${err.whyNoMatchStr(arg.tpe, bound)}",
          arg.pos)

  /** Check that type arguments `args` conform to corresponding bounds in `poly`
   *  Note: This does not check the bounds of AppliedTypeTrees. These
   *  are handled by method checkBounds in FirstTransform
   */
  def checkBounds(args: List[tpd.Tree], poly: PolyType)(implicit ctx: Context): Unit =
    checkBounds(args, poly.paramBounds, _.substParams(poly, _))

  /** Traverse type tree, performing the following checks:
   *  1. All arguments of applied type trees must conform to their bounds.
   *  2. Prefixes of type selections and singleton types must be realizable.
   */
  val typeChecker = new TreeTraverser {
    def traverse(tree: Tree)(implicit ctx: Context) = {
      tree match {
        case AppliedTypeTree(tycon, args) =>
          // If `args` is a list of named arguments, return corresponding type parameters,
          // otherwise return type parameters unchanged
          def matchNamed(tparams: List[TypeSymbol], args: List[Tree]): List[Symbol] =
            if (hasNamedArg(args))
              for (NamedArg(name, _) <- args) yield tycon.tpe.member(name).symbol
            else
              tparams
          val tparams = matchNamed(tycon.tpe.typeSymbol.typeParams, args)
          val bounds = tparams.map(tparam =>
            tparam.info.asSeenFrom(tycon.tpe.normalizedPrefix, tparam.owner.owner).bounds)
          checkBounds(args, bounds, _.substDealias(tparams, _))
        case Select(qual, name) if name.isTypeName =>
          checkRealizable(qual.tpe, qual.pos)
        case SelectFromTypeTree(qual, name) if name.isTypeName =>
          checkRealizable(qual.tpe, qual.pos)
        case SingletonTypeTree(ref) =>
          checkRealizable(ref.tpe, ref.pos)
        case _ =>
      }
      traverseChildren(tree)
    }
  }

  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  def checkInstantiable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.is(AbstractOrTrait))
          ctx.error(d"$cls is abstract; cannot be instantiated", pos)
        if (!cls.is(Module)) {
          // Create a synthetic singleton type instance, and check whether
          // it conforms to the self type of the class as seen from that instance.
          val stp = SkolemType(tp)
          val selfType = tref.givenSelfType.asSeenFrom(stp, cls)
          if (selfType.exists && !(stp <:< selfType))
            ctx.error(d"$tp does not conform to its self type $selfType; cannot be instantiated")
        }
      case _ =>
    }

  /** Check that type `tp` is realizable. */
  def checkRealizable(tp: Type, pos: Position)(implicit ctx: Context): Unit = {
    val rstatus = realizability(tp)
    if (rstatus ne Realizable) {
      def msg = d"$tp is not a legal path\n since it${rstatus.msg}"
      if (ctx.scala2Mode) ctx.migrationWarning(msg, pos) else ctx.error(msg, pos)
    }
  }

  /** A type map which checks that the only cycles in a type are F-bounds
   *  and that protects all F-bounded references by LazyRefs.
   */
  class CheckNonCyclicMap(sym: Symbol, reportErrors: Boolean)(implicit ctx: Context) extends TypeMap {

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
      case tp @ TypeRef(pre, name) =>
        try {
          // A prefix is interesting if it might contain (transitively) a reference
          // to symbol `sym` itself. We only check references with interesting
          // prefixes for cycles. This pruning is done in order not to force
          // global symbols when doing the cyclicity check.
          def isInteresting(prefix: Type): Boolean = prefix.stripTypeVar match {
            case NoPrefix => true
            case prefix: ThisType => sym.owner.isClass && prefix.cls.isContainedIn(sym.owner)
            case prefix: NamedType => !prefix.symbol.isStaticOwner && isInteresting(prefix.prefix)
            case SuperType(thistp, _) => isInteresting(thistp)
            case AndType(tp1, tp2) => isInteresting(tp1) || isInteresting(tp2)
            case OrType(tp1, tp2) => isInteresting(tp1) && isInteresting(tp2)
            case _: RefinedOrRecType => true
            case _ => false
          }
          if (isInteresting(pre)) {
            val pre1 = this(pre, false, false)
            checkInfo(tp.info)
            if (pre1 eq pre) tp else tp.newLikeThis(pre1)
          }
          else tp
        } catch {
          case ex: CyclicReference =>
            ctx.debuglog(i"cycle detected for $tp, $nestedCycleOK, $cycleOK")
            if (cycleOK) LazyRef(() => tp)
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
          ctx.error(i"illegal cyclic reference: ${checker.where} ${checker.lastChecked} of $sym refers back to the type itself", sym.pos)
          ErrorType
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
    //println(i"check wf $sym with flags ${sym.flags}")
    def fail(msg: String) = ctx.error(msg, sym.pos)
    def varNote =
      if (sym.is(Mutable)) "\n(Note that variables need to be initialized to be defined)"
      else ""

    def checkWithDeferred(flag: FlagSet) =
      if (sym.is(flag))
        fail(i"abstract member may not have `$flag' modifier")
    def checkNoConflict(flag1: FlagSet, flag2: FlagSet) =
      if (sym.is(allOf(flag1, flag2)))
        fail(i"illegal combination of modifiers: $flag1 and $flag2 for: $sym")

    if (sym.is(ImplicitCommon)) {
      if (sym.owner.is(Package))
        fail(i"`implicit' modifier cannot be used for top-level definitions")
      if (sym.isType)
        fail(i"`implicit' modifier cannot be used for types or traits")
    }
    if (!sym.isClass && sym.is(Abstract))
      fail(i"`abstract' modifier can be used only for classes; it should be omitted for abstract members")
    if (sym.is(AbsOverride) && !sym.owner.is(Trait))
      fail(i"`abstract override' modifier only allowed for members of traits")
    if (sym.is(Trait) && sym.is(Final))
      fail(i"$sym may not be `final'")
    if (sym.hasAnnotation(defn.NativeAnnot)) {
      if (!sym.is(Deferred))
        fail(i"`@native' members may not have implementation")
    }
    else if (sym.is(Deferred, butNot = Param) && !sym.isSelfSym) {
      if (!sym.owner.isClass || sym.owner.is(Module) || sym.owner.isAnonymousClass)
        fail(i"only classes can have declared but undefined members$varNote")
      checkWithDeferred(Private)
      checkWithDeferred(Final)
    }
    if (sym.isValueClass && sym.is(Trait) && !sym.isRefinementClass)
      fail(i"$sym cannot extend AnyVal")
    checkNoConflict(Final, Sealed)
    checkNoConflict(Private, Protected)
    checkNoConflict(Abstract, Override)
  }

  /** Check the type signature of the symbol `M` defined by `tree` does not refer
   *  to a private type or value which is invisible at a point where `M` is still
   *  visible. As an exception, we allow references to type aliases if the underlying
   *  type of the alias is not a leak. So type aliases are transparent as far as
   *  leak testing is concerned.
   *  @return The `info` of `sym`, with problematic aliases expanded away.
   *  See i997.scala for tests, i1130.scala for a case where it matters that we
   *  transform leaky aliases away.
   */
  def checkNoPrivateLeaks(sym: Symbol, pos: Position)(implicit ctx: Context): Type = {
    class NotPrivate extends TypeMap {
      type Errors = List[(String, Position)]
      var errors: Errors = Nil
      def accessBoundary(sym: Symbol): Symbol =
        if (sym.is(Private)) sym.owner
        else if (sym.privateWithin.exists) sym.privateWithin
        else if (sym.is(Package)) sym
        else accessBoundary(sym.owner)
      def apply(tp: Type): Type = tp match {
        case tp: NamedType =>
          val prevErrors = errors
          var tp1 =
            if (tp.symbol.is(Private) &&
                !accessBoundary(sym).isContainedIn(tp.symbol.owner)) {
              errors = (d"non-private $sym refers to private ${tp.symbol}\n in its type signature ${sym.info}",
                        pos) :: errors
              tp
            }
            else mapOver(tp)
          if ((errors ne prevErrors) && tp.info.isAlias) {
            // try to dealias to avoid a leak error
            val savedErrors = errors
            errors = prevErrors
            val tp2 = apply(tp.info.bounds.hi)
            if (errors eq prevErrors) tp1 = tp2
            else errors = savedErrors
          }
          tp1
        case tp: ClassInfo =>
          tp.derivedClassInfo(
            prefix = apply(tp.prefix),
            classParents = tp.parentsWithArgs.map(p =>
              apply(p).underlyingClassRef(refinementOK = false).asInstanceOf[TypeRef]))
        case _ =>
          mapOver(tp)
      }
    }
    val notPrivate = new NotPrivate
    val info = notPrivate(sym.info)
    notPrivate.errors.foreach { case (msg, pos) => ctx.errorOrMigrationWarning(msg, pos) }
    info
  }
}

trait Checking {

  import tpd._

  def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(implicit ctx: Context): Type =
    Checking.checkNonCyclic(sym, info, reportErrors)

  /** Check that Java statics and packages can only be used in selections.
   */
  def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = {
    if (!proto.isInstanceOf[SelectionProto]) {
      val sym = tree.tpe.termSymbol
      // The check is avoided inside Java compilation units because it always fails
      // on the singleton type Module.type.
      if ((sym is Package) || ((sym is JavaModule) && !ctx.compilationUnit.isJava)) ctx.error(d"$sym is not a value", tree.pos)
    }
    tree
  }

  /** Check that type `tp` is stable. */
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(d"$tp is not stable", pos)

  /** Check that all type members of `tp` have realizable bounds */
  def checkRealizableBounds(tp: Type, pos: Position)(implicit ctx: Context): Unit = {
    val rstatus = boundsRealizability(tp)
    if (rstatus ne Realizable)
      ctx.error(i"$tp cannot be instantiated since it${rstatus.msg}", pos)
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
        if (traitReq && !(tref.symbol is Trait)) ctx.error(d"$tref is not a trait", pos)
        if (stablePrefixReq && ctx.phase <= ctx.refchecksPhase) checkStable(tref.prefix, pos)
        tp
      case _ =>
        ctx.error(d"$tp is not a class type", pos)
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
      ctx.error(d"no type exists between low bound $lo and high bound $hi$where", pos)
      TypeAlias(hi)
    case _ =>
      tp
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
            def ofType = if (decl.isType) "" else d": ${other.info}"
            def explanation =
              if (!decl.isRealMethod) ""
              else "\n (the definitions have matching type signatures)"
            ctx.error(d"$decl is already defined as $other$ofType$explanation", decl.pos)
          }
          if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if ((decl is HasDefaultParams) && (other is HasDefaultParams)) {
          ctx.error(d"two or more overloaded variants of $decl have default arguments")
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
                   |its constructor cannot be called again""".stripMargin, call.pos)
    }

  /** Check that `tpt` does not define a higher-kinded type */
  def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree =
    if (tpt.tpe.isHK && !ctx.compilationUnit.isJava) {
        // be more lenient with missing type params in Java,
        // needed to make pos/java-interop/t1196 work.
      errorTree(tpt, d"missing type parameter for ${tpt.tpe}")
    }
    else tpt

  def checkLowerNotHK(sym: Symbol, tparams: List[Symbol], pos: Position)(implicit ctx: Context) =
    if (tparams.nonEmpty)
      sym.info match {
        case info: TypeAlias => // ok
        case TypeBounds(lo, _) =>
          for (tparam <- tparams)
            if (tparam.typeRef.occursIn(lo))
              ctx.error(i"type parameter ${tparam.name} may not occur in lower bound $lo", pos)
        case _ =>
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
  override def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = ()
  override def checkParentCall(call: Tree, caller: ClassSymbol)(implicit ctx: Context) = ()
  override def checkSimpleKinded(tpt: Tree)(implicit ctx: Context): Tree = tpt
  override def checkLowerNotHK(sym: Symbol, tparams: List[Symbol], pos: Position)(implicit ctx: Context) = ()
}
