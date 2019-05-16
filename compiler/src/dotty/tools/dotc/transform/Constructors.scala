package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import ast._
import Trees._
import Flags._
import SymUtils._
import Symbols._
import Decorators._
import DenotTransformers._
import Constants.Constant
import collection.mutable

object Constructors {
  val name: String = "constructors"
}

/** This transform
 *   - moves initializers from body to constructor.
 *   - makes all supercalls explicit
 *   - also moves private fields that are accessed only from constructor
 *     into the constructor if possible.
 */
class Constructors extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = Constructors.name
  override def runsAfter: Set[String] = Set(HoistSuperArgs.name)
  override def runsAfterGroupsOf: Set[String] = Set(Memoize.name)
    // Memoized needs to be finished because we depend on the ownerchain after Memoize
    // when checking whether an ident is an access in a constructor or outside it.
    // This test is done in the right-hand side of a value definition. If Memoize
    // was in the same group as Constructors, the test on the rhs ident would be
    // performed before the rhs undergoes the owner change. This would lead
    // to more symbols being retained as parameters. Test case in run/capturing.scala.

  /** The private vals that are known to be retained as class fields */
  private val retainedPrivateVals = mutable.Set[Symbol]()

  /** The private vals whose definition comes before the current focus */
  private val seenPrivateVals = mutable.Set[Symbol]()

  // Collect all private parameter accessors and value definitions that need
  // to be retained. There are several reasons why a parameter accessor or
  // definition might need to be retained:
  // 1. It is accessed after the constructor has finished
  // 2. It is accessed before it is defined
  // 3. It is accessed on an object other than `this`
  // 4. It is a mutable parameter accessor
  // 5. It is has a wildcard initializer `_`
  private def markUsedPrivateSymbols(tree: RefTree)(implicit ctx: Context): Unit = {

    val sym = tree.symbol
    def retain() = retainedPrivateVals.add(sym)

    if (sym.exists && sym.owner.isClass && mightBeDropped(sym)) {
      val owner = sym.owner.asClass

        tree match {
          case Ident(_) | Select(This(_), _) =>
            def inConstructor = {
              val method = ctx.owner.enclosingMethod
              method.isPrimaryConstructor && ctx.owner.enclosingClass == owner
            }
            if (inConstructor &&
                (sym.is(ParamAccessor) || seenPrivateVals.contains(sym))) {
              // used inside constructor, accessed on this,
              // could use constructor argument instead, no need to retain field
            }
            else retain()
          case _ => retain()
        }
    }
  }

  override def transformIdent(tree: tpd.Ident)(implicit ctx: Context): tpd.Tree = {
    markUsedPrivateSymbols(tree)
    tree
  }

  override def transformSelect(tree: tpd.Select)(implicit ctx: Context): tpd.Tree = {
    markUsedPrivateSymbols(tree)
    tree
  }

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context): tpd.Tree = {
    if (mightBeDropped(tree.symbol)) seenPrivateVals += tree.symbol
    tree
  }

  /** All initializers for non-lazy fields should be moved into constructor.
   *  All non-abstract methods should be implemented (this is assured for constructors
   *  in this phase and for other methods in memoize).
   */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    def emptyRhsOK(sym: Symbol) =
      sym.is(LazyOrDeferred) || sym.isConstructor && sym.owner.is(NoInitsTrait)
    tree match {
      case tree: ValDef if tree.symbol.exists && tree.symbol.owner.isClass && !tree.symbol.is(Lazy) && !tree.symbol.hasAnnotation(defn.ScalaStaticAnnot) =>
        assert(tree.rhs.isEmpty, i"$tree: initializer should be moved to constructors")
      case tree: DefDef if !emptyRhsOK(tree.symbol) =>
        assert(!tree.rhs.isEmpty, i"unimplemented: $tree")
      case _ =>
    }
  }

  /** @return true  if after ExplicitOuter, all references from this tree go via an
   *                outer link, so no parameter accessors need to be rewired to parameters
   */
  private def noDirectRefsFrom(tree: Tree)(implicit ctx: Context) =
    tree.isDef && tree.symbol.isClass

  /** Class members that can be eliminated if referenced only from their own
   *  constructor.
   */
  private def mightBeDropped(sym: Symbol)(implicit ctx: Context) =
    sym.is(Private, butNot = MethodOrLazy) && !sym.is(MutableParamAccessor)

  private final val MutableParamAccessor = allOf(Mutable, ParamAccessor)

  override def transformTemplate(tree: Template)(implicit ctx: Context): Tree = {
    val cls = ctx.owner.asClass

    val constr @ DefDef(nme.CONSTRUCTOR, Nil, vparams :: Nil, _, EmptyTree) = tree.constr

    // Produce aligned accessors and constructor parameters. We have to adjust
    // for any outer parameters, which are last in the sequence of original
    // parameter accessors but come first in the constructor parameter list.
    val accessors = cls.paramAccessors.filterNot(x => x.isSetter)
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
          if (sym.owner.isConstructor) ref(sym).withSpan(tree.span) else tree
        case Apply(fn, Nil) =>
          val fn1 = transform(fn)
          if ((fn1 ne fn) && fn1.symbol.is(Param) && fn1.symbol.owner.isPrimaryConstructor)
            fn1 // in this case, fn1.symbol was an alias for a parameter in a superclass
          else cpy.Apply(tree)(fn1, Nil)
        case _ =>
          if (noDirectRefsFrom(tree)) tree else super.transform(tree)
      }

      def apply(tree: Tree, prevOwner: Symbol)(implicit ctx: Context): Tree = {
        transform(tree).changeOwnerAfter(prevOwner, constr.symbol, thisPhase)
      }
    }

    def isRetained(acc: Symbol) = {
      !mightBeDropped(acc) || retainedPrivateVals(acc)
    }

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

    val dropped = mutable.Set[Symbol]()

    // Split class body into statements that go into constructor and
    // definitions that are kept as members of the class.
    def splitStats(stats: List[Tree]): Unit = stats match {
      case stat :: stats1 =>
        stat match {
          case stat @ ValDef(name, tpt, _) if !stat.symbol.is(Lazy) && !stat.symbol.hasAnnotation(defn.ScalaStaticAnnot) =>
            val sym = stat.symbol
            if (isRetained(sym)) {
              if (!stat.rhs.isEmpty && !isWildcardArg(stat.rhs))
                constrStats += Assign(ref(sym), intoConstr(stat.rhs, sym)).withSpan(stat.span)
              clsStats += cpy.ValDef(stat)(rhs = EmptyTree)
            }
            else if (!stat.rhs.isEmpty) {
              dropped += sym
              sym.copySymDenotation(
                initFlags = sym.flags &~ Private,
                owner = constr.symbol).installAfter(thisPhase)
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
      if (!isRetained(acc)) {
        dropped += acc
        Nil
      } else {
        if (acc.hasAnnotation(defn.TransientParamAnnot))
          ctx.error(em"transient parameter $acc is retained as field in class ${acc.owner}", acc.sourcePos)
        val target = if (acc.is(Method)) acc.field else acc
        if (!target.exists) Nil // this case arises when the parameter accessor is an alias
        else {
          val param = acc.subst(accessors, paramSyms)
          val assigns = Assign(ref(target), ref(param)).withSpan(tree.span) :: Nil
          if (acc.name != nme.OUTER) assigns
          else {
            // insert test: if ($outer eq null) throw new NullPointerException
            val nullTest =
              If(ref(param).select(defn.Object_eq).appliedTo(nullLiteral),
                 Throw(New(defn.NullPointerExceptionClass.typeRef, Nil)),
                 unitLiteral)
            nullTest :: assigns
          }
        }
      }
    }

    // Drop accessors that are not retained from class scope
    if (dropped.nonEmpty) {
      val clsInfo = cls.classInfo
      cls.copy(
        info = clsInfo.derivedClassInfo(
          decls = clsInfo.decls.filteredScope(!dropped.contains(_))))

      // TODO: this happens to work only because Constructors is the last phase in group
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

    // Lazy Vals may decide to create an eager val instead of a lazy val
    // this val should be assigned before constructor body code starts running

    val (lazyAssignments, stats) = followConstrStats.partition {
      case Assign(l, r) if l.symbol.name.is(NameKinds.LazyLocalName) => true
      case _ => false
    }

    val finalConstrStats = copyParams ::: mappedSuperCalls ::: lazyAssignments ::: stats
    val expandedConstr =
      if (cls.is(NoInitsTrait)) {
        assert(finalConstrStats.isEmpty)
        constr
      }
      else cpy.DefDef(constr)(rhs = Block(finalConstrStats, unitLiteral))

    cpy.Template(tree)(constr = expandedConstr, body = clsStats.toList)
  }
}
