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
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import util.common._
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
  def checkBounds(args: List[tpd.Tree], bounds: List[TypeBounds], instantiate: (Type, List[Type]) => Type)(implicit ctx: Context) = {
    val argTypes = args.tpes
    for ((arg, bounds) <- args zip bounds) {
      def notConforms(which: String, bound: Type) = {
        ctx.error(
          d"Type argument ${arg.tpe} does not conform to $which bound $bound ${err.whyNoMatchStr(arg.tpe, bound)}",
          arg.pos)
      }
      def checkOverlapsBounds(lo: Type, hi: Type): Unit = {
        //println(i"instantiating ${bounds.hi} with $argTypes")
        //println(i" = ${instantiate(bounds.hi, argTypes)}")
        val hiBound = instantiate(bounds.hi, argTypes)
        if (!(lo <:< hiBound)) notConforms("upper", hiBound)
        if (!(bounds.lo <:< hi)) notConforms("lower", bounds.lo)
      }
      arg.tpe match {
        case TypeBounds(lo, hi) => checkOverlapsBounds(lo, hi)
        case tp => checkOverlapsBounds(tp, tp)
      }
    }
  }

  /** A type map which checks that the only cycles in a type are F-bounds
   *  and that protects all F-bounded references by LazyRefs.
   */
  class CheckNonCyclicMap(sym: Symbol, reportErrors: Boolean)(implicit ctx: Context) extends TypeMap {

    /** Are cycles allowed within nested refinedInfos of currently checked type? */
    private var nestedCycleOK = false

    /** Are cycles allwoed within currently checked type? */
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
      case tp @ TypeBounds(lo, hi) =>
        if (lo eq hi)
          try tp.derivedTypeAlias(apply(lo))
          finally {
            where = "alias"
            lastChecked = lo
          }
        else {
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
        }
      case _ =>
        tp
    }

    def apply(tp: Type) = tp match {
      case tp @ RefinedType(parent, name) =>
        val parent1 = this(parent)
        val saved = cycleOK
        cycleOK = nestedCycleOK
        try tp.derivedRefinedType(parent1, name, this(tp.refinedInfo))
        finally cycleOK = saved
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
            case _ => false
          }
          // If prefix is interesting, check info of typeref recursively, marking the referred symbol
          // with NoCompleter. This provokes a CyclicReference when the symbol
          // is hit again. Without this precaution we could stackoverflow here.
          if (isInteresting(pre)) {
            val info = tp.info
            val symInfo = tp.symbol.info
            if (tp.symbol.exists) tp.symbol.info = SymDenotations.NoCompleter
            try checkInfo(info)
            finally if (tp.symbol.exists) tp.symbol.info = symInfo
          }
          tp
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
    val checker = new CheckNonCyclicMap(sym, reportErrors)(ctx.withMode(Mode.CheckCyclic))
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
      if ((sym is Package) || (sym is JavaModule)) ctx.error(d"$sym is not a value", tree.pos)
    }
    tree
  }

  /** Check that type arguments `args` conform to corresponding bounds in `poly`
   *  Note: This does not check the bounds of AppliedTypeTrees. These
   *  are handled by method checkBounds in FirstTransform
   *  TODO: remove pos parameter
   */
  def checkBounds(args: List[tpd.Tree], poly: PolyType, pos: Position)(implicit ctx: Context): Unit =    Checking.checkBounds(
      args, poly.paramBounds, (tp, argTypes) => tp.substParams(poly, argTypes))

  /** Check that type `tp` is stable. */
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(d"$tp is not stable", pos)

  /** Check that type `tp` is a legal prefix for '#'.
   *  @return The type itself
   */
  def checkLegalPrefix(tp: Type, selector: Name, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isLegalPrefixFor(selector)) ctx.error(d"$tp is not a valid prefix for '# $selector'", pos)

 /**  Check that `tp` is a class type with a stable prefix. Also, if `traitReq` is
   *  true check that `tp` is a trait.
   *  Stability checking is disabled in phases after RefChecks.
   *  @return  `tp` itself if it is a class or trait ref, ObjectClass.typeRef if not.
   */
  def checkClassTypeWithStablePrefix(tp: Type, pos: Position, traitReq: Boolean)(implicit ctx: Context): Type =
    tp.underlyingClassRef match {
      case tref: TypeRef =>
        if (ctx.phase <= ctx.refchecksPhase) checkStable(tref.prefix, pos)
        if (traitReq && !(tref.symbol is Trait)) ctx.error(d"$tref is not a trait", pos)
        tp
      case _ =>
        ctx.error(d"$tp is not a class type", pos)
        defn.ObjectClass.typeRef
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
   *  their lower bound conforms to their upper cound. If a type argument is
   *  infeasible, issue and error and continue with upper bound.
   */
  def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp match {
    case tp: RefinedType =>
      tp.derivedRefinedType(tp.parent, tp.refinedName, checkFeasible(tp.refinedInfo, pos, where))
    case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
      ctx.error(d"no type exists between low bound $lo and high bound $hi$where", pos)
      tp.derivedTypeAlias(hi)
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
        if (decl.signature matches other.signature) {
          def doubleDefError(decl: Symbol, other: Symbol): Unit = {
            def ofType = if (decl.isType) "" else d": ${other.info}"
            def explanation =
              if (!decl.isSourceMethod) ""
              else "\n (both definitions have the same erased type signature)"
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

  def checkInstantiatable(cls: ClassSymbol, pos: Position): Unit = {
    ??? // to be done in later phase: check that class `cls` is legal in a new.
  }
}

trait NoChecking extends Checking {
  import tpd._
  override def checkNonCyclic(sym: Symbol, info: TypeBounds, reportErrors: Boolean)(implicit ctx: Context): Type = info
  override def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = tree
  override def checkBounds(args: List[tpd.Tree], poly: PolyType, pos: Position)(implicit ctx: Context): Unit = ()
  override def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit = ()
  override def checkLegalPrefix(tp: Type, selector: Name, pos: Position)(implicit ctx: Context): Unit = ()
  override def checkClassTypeWithStablePrefix(tp: Type, pos: Position, traitReq: Boolean)(implicit ctx: Context): Type = tp
  override def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = ()
  override def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp
  override def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = ()
}