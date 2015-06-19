package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import Phases._
import ast._
import Trees._
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import util.Positions._
import Constants.Constant
import collection.mutable

/** This transform
 *   - moves initializers from body to constructor.
 *   - makes all supercalls explicit
 *   - also moves private fields that are accessed only from constructor
 *     into the constructor if possible.
 */
class Constructors extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import tpd._

  override def phaseName: String = "constructors"
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Memoize])


  /** All initializers for non-lazy fields should be moved into constructor.
   *  All non-abstract methods should be implemented (this is assured for constructors
   *  in this phase and for other methods in memoize).
   */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    tree match {
      case tree: ValDef if tree.symbol.exists && tree.symbol.owner.isClass && !tree.symbol.is(Lazy) =>
        assert(tree.rhs.isEmpty, i"$tree: initializer should be moved to constructors")
      case tree: DefDef if !tree.symbol.is(LazyOrDeferred) =>
        assert(!tree.rhs.isEmpty, i"unimplemented: $tree")
      case _ =>
    }
  }

  /** Symbols that are owned by either <local dummy> or a class field move into the
   *  primary constructor.
   */
  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def ownerBecomesConstructor(owner: Symbol): Boolean =
      (owner.isLocalDummy || owner.isTerm && !owner.is(MethodOrLazy)) &&
      owner.owner.isClass
    if (ownerBecomesConstructor(sym.owner))
      sym.copySymDenotation(owner = sym.owner.enclosingClass.primaryConstructor)
    else sym
  }

  /** @return true  if after ExplicitOuter, all references from this tree go via an
   *                outer link, so no parameter accessors need to be rewired to parameters
   */
  private def noDirectRefsFrom(tree: Tree)(implicit ctx: Context) =
    tree.isDef && tree.symbol.isClass && !tree.symbol.is(InSuperCall)

  /** Class members that can be eliminated if referenced only from their own
   *  constructor.
   */
  private def mightBeDropped(sym: Symbol)(implicit ctx: Context) =
    sym.is(Private, butNot = MethodOrLazy) && !sym.is(MutableParamAccessor)

  private final val MutableParamAccessor = allOf(Mutable, ParamAccessor)

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass
    val constr @ DefDef(nme.CONSTRUCTOR, Nil, vparams :: Nil, _, EmptyTree) = tree.constr

    // Produce aligned accessors and constructor parameters. We have to adjust
    // for any outer parameters, which are last in the sequence of original
    // parameter accessors but come first in the constructor parameter list.
    val accessors = cls.paramAccessors.filterNot(_.isSetter)
    val vparamsWithOuterLast = vparams match {
      case vparam :: rest if vparam.name == nme.OUTER => rest ::: vparam :: Nil
      case _ => vparams
    }
    val paramSyms = vparamsWithOuterLast map (_.symbol)

    // Adjustments performed when moving code into the constructor:
    //  (1) Replace references to param accessors by constructor parameters
    //      except possibly references to mutable variables, if `excluded = Mutable`.
    //      (Mutable parameters should be replaced only during the super call)
    //  (2) If the parameter accessor reference was to an alias getter,
    //      drop the () when replacing by the parameter.
    object intoConstr extends TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case Ident(_) | Select(This(_), _) =>
          var sym = tree.symbol
          if (sym is (ParamAccessor, butNot = Mutable)) sym = sym.subst(accessors, paramSyms)
          if (sym.owner.isConstructor) ref(sym).withPos(tree.pos) else tree
        case Apply(fn, Nil) =>
          val fn1 = transform(fn)
          if ((fn1 ne fn) && fn1.symbol.is(Param) && fn1.symbol.owner.isPrimaryConstructor)
            fn1 // in this case, fn1.symbol was an alias for a parameter in a superclass
          else cpy.Apply(tree)(fn1, Nil)
        case _ =>
          if (noDirectRefsFrom(tree)) tree else super.transform(tree)
      }

      def apply(tree: Tree, prevOwner: Symbol)(implicit ctx: Context): Tree = {
        transform(tree).changeOwnerAfter(prevOwner, constr.symbol, thisTransform)
      }
    }

    // Collect all private parameter accessors and value definitions that need
    // to be retained. There are several reasons why a parameter accessor or
    // definition might need to be retained:
    // 1. It is accessed after the constructor has finished
    // 2. It is accessed before it is defined
    // 3. It is accessed on an object other than `this`
    // 4. It is a mutable parameter accessor
    // 5. It is has a wildcard initializer `_`
    object usage extends TreeTraverser {
      private var inConstr: Boolean = true
      private val seen = mutable.Set[Symbol](accessors: _*)
      val retained = mutable.Set[Symbol]()
      def dropped: collection.Set[Symbol] = seen -- retained
      override def traverse(tree: Tree)(implicit ctx: Context) = {
        val sym = tree.symbol
        tree match {
          case Ident(_) | Select(This(_), _) if inConstr && seen(tree.symbol) =>
            // could refer to definition in constructors, so no retention necessary
          case tree: RefTree =>
            if (mightBeDropped(sym)) retained += sym
          case _ =>
        }
        if (!noDirectRefsFrom(tree)) traverseChildren(tree)
      }
      def collect(stats: List[Tree]): Unit = stats foreach {
        case stat: ValDef if !stat.symbol.is(Lazy) =>
          traverse(stat)
          if (mightBeDropped(stat.symbol))
            (if (isWildcardStarArg(stat.rhs)) retained else seen) += stat.symbol
        case stat: DefTree =>
          inConstr = false
          traverse(stat)
          inConstr = true
        case stat =>
          traverse(stat)
      }
    }
    usage.collect(tree.body)

    def isRetained(acc: Symbol) = !mightBeDropped(acc) || usage.retained(acc)

    val constrStats, clsStats = new mutable.ListBuffer[Tree]

    /** Map outer getters $outer and outer accessors $A$B$$$outer to the given outer parameter. */
    def mapOuter(outerParam: Symbol) = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context) = tree match {
        case Apply(fn, Nil)
          if (fn.symbol.is(OuterAccessor)
             || fn.symbol.isGetter && fn.symbol.name == nme.OUTER
             ) &&
             fn.symbol.info.resultType.classSymbol == outerParam.info.classSymbol =>
          ref(outerParam)
        case _ =>
          super.transform(tree)
      }
    }

    // Split class body into statements that go into constructor and
    // definitions that are kept as members of the class.
    def splitStats(stats: List[Tree]): Unit = stats match {
      case stat :: stats1 =>
        stat match {
          case stat @ ValDef(name, tpt, _) if !stat.symbol.is(Lazy) =>
            val sym = stat.symbol
            if (isRetained(sym)) {
              if (!stat.rhs.isEmpty && !isWildcardArg(stat.rhs))
                constrStats += Assign(ref(sym), intoConstr(stat.rhs, sym)).withPos(stat.pos)
              clsStats += cpy.ValDef(stat)(rhs = EmptyTree)
            }
            else if (!stat.rhs.isEmpty) {
              sym.copySymDenotation(
                initFlags = sym.flags &~ Private,
                owner = constr.symbol).installAfter(thisTransform)
              constrStats += intoConstr(stat, sym)
            }
          case DefDef(nme.CONSTRUCTOR, _, ((outerParam @ ValDef(nme.OUTER, _, _)) :: _) :: Nil, _, _) =>
            clsStats += mapOuter(outerParam.symbol).transform(stat)
          case _: DefTree =>
            clsStats += stat
          case _ =>
            constrStats += intoConstr(stat, tree.symbol)
        }
        splitStats(stats1)
      case Nil =>
        (Nil, Nil)
    }
    splitStats(tree.body)

    // The initializers for the retained accessors */
    val copyParams = accessors flatMap { acc =>
      if (!isRetained(acc)) Nil
      else {
        val target = if (acc.is(Method)) acc.field else acc
        if (!target.exists) Nil // this case arises when the parameter accessor is an alias
        else {
          val param = acc.subst(accessors, paramSyms)
          val assigns = Assign(ref(target), ref(param)).withPos(tree.pos) :: Nil
          if (acc.name != nme.OUTER) assigns
          else {
            // insert test: if ($outer eq null) throw new NullPointerException
            val nullTest =
              If(ref(param).select(defn.Object_eq).appliedTo(Literal(Constant(null))),
                 Throw(New(defn.NullPointerExceptionClass.typeRef, Nil)),
                 unitLiteral)
            nullTest :: assigns
          }
        }
      }
    }

    // Drop accessors that are not retained from class scope
    val dropped = usage.dropped
    if (dropped.nonEmpty) {
      val clsInfo = cls.classInfo // TODO investigate: expand clsInfo to cls.info => dotty type error
      cls.copy(
        info = clsInfo.derivedClassInfo(
          decls = clsInfo.decls.filteredScope(!dropped.contains(_))))
    }

    val (superCalls, followConstrStats) = constrStats.toList match {
      case (sc: Apply) :: rest if sc.symbol.isConstructor => (sc :: Nil, rest)
      case stats => (Nil, stats)
    }

    val mappedSuperCalls = vparams match {
      case (outerParam @ ValDef(nme.OUTER, _, _)) :: _ =>
        superCalls.map(mapOuter(outerParam.symbol).transform)
      case _ => superCalls
    }

    cpy.Template(tree)(
      constr = cpy.DefDef(constr)(
        rhs = Block(mappedSuperCalls ::: copyParams ::: followConstrStats, unitLiteral)),
      body = clsStats.toList)
  }
}
