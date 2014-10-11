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
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  override def treeTransformPhase = thisTransform.next

  /** Symbols that are owned by either <local dummy> or a class field move into the
   *  primary constructor.
   */
  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def ownerBecomesConstructor(owner: Symbol): Boolean =
      (owner.isLocalDummy || owner.isTerm && !owner.is(Method | Lazy)) &&
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
    sym.is(Private, butNot = KeeperFlags) && !sym.is(MutableParamAccessor)

  private final val KeeperFlags = Method | Lazy | NotJavaPrivate
  private final val MutableParamAccessor = allOf(Mutable, ParamAccessor)

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass
    val constr @ DefDef(_, nme.CONSTRUCTOR, Nil, vparams :: Nil, _, EmptyTree) = tree.constr

    // Produce aligned accessors and constructor parameters. We have to adjust
    // for any outer parameters, which are last in the sequence of original
    // parameter accessors but should come first in the constructor parameter list.
    var accessors = cls.paramAccessors.filterNot(_.isSetter)
    var vparamsWithOuter = vparams
    if (!accessors.hasSameLengthAs(vparams)) {
      accessors.reverse match {
        case last :: _ if (last.name == nme.OUTER) =>
          accessors = last :: accessors.init // align wth calling convention
          vparamsWithOuter = ValDef(last.asTerm) :: vparams
        case _ =>
      }
      assert(accessors.hasSameLengthAs(vparamsWithOuter),
        i"lengths differ for $cls, param accs = $accessors, params = ($vparamsWithOuter%, %)")
    }
    val paramSyms = vparamsWithOuter map (_.symbol)

    // Adjustments performed when moving code into the constructor:
    //  (1) Replace references to param accessors by constructor parameters
    //      except possibly references to mutable variables, if `excluded = Mutable`.
    //      (Mutable parameters should be replaced only during the super call)
    //  (2) If the parameter accessor reference was to an alias getter,
    //      drop the () when replacing by the parameter.
    object intoConstr extends TreeMap {
      private var excluded: FlagSet = _
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case Ident(_) | Select(This(_), _) =>
          var sym = tree.symbol
          if (sym is (ParamAccessor, butNot = excluded)) sym = sym.subst(accessors, paramSyms)
          if (sym.owner.isConstructor) ref(sym).withPos(tree.pos) else tree
        case Apply(fn, Nil) =>
          val fn1 = transform(fn)
          if ((fn1 ne fn) && fn1.symbol.is(Param) && fn1.symbol.owner.isPrimaryConstructor)
            fn1 // in this case, fn1.symbol was an alias for a parameter in a superclass
          else cpy.Apply(tree)(fn1, Nil)
        case _ =>
          if (noDirectRefsFrom(tree)) tree else super.transform(tree)
      }

      def apply(tree: Tree, inSuperCall: Boolean = false)(implicit ctx: Context): Tree = {
        this.excluded = if (inSuperCall) EmptyFlags else Mutable
        transform(tree)
      }
    }

    val superCalls = new mutable.ListBuffer[Tree]

    // If parent is a constructor call, pull out the call into a separate
    // supercall constructor, which gets appended to `superCalls`, and keep
    // only the type.
    def normalizeParent(tree: Tree) = tree match {
      case superApp @ Apply(
        superSel @ Select(
          superNew @ New(superType),
          nme.CONSTRUCTOR),
        superArgs) =>
        val toClass = !superType.symbol.is(Trait)
        val mappedArgs = superArgs.map(intoConstr(_, inSuperCall = toClass))
        val receiver =
          if (toClass) Super(This(cls), tpnme.EMPTY, inConstrCall = true)
          else This(cls)
        superCalls +=
          cpy.Apply(superApp)(
            receiver.withPos(superNew.pos)
              .select(superSel.symbol).withPos(superSel.pos),
            mappedArgs)
        superType
      case tree: TypeTree => tree
    }
    val parentTypeTrees = tree.parents.map(normalizeParent)

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
      override def traverse(tree: Tree) = {
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
    usage.collect(superCalls.toList ++ tree.body)

    def isRetained(acc: Symbol) = !mightBeDropped(acc) || usage.retained(acc)

    val constrStats, clsStats = new mutable.ListBuffer[Tree]

    def assign(vble: Symbol, rhs: Tree): Tree =
      if (cls is Trait) ref(vble.setter).appliedTo(rhs)
      else Assign(ref(vble), rhs)

    // Split class body into statements that go into constructor and
    // definitions that are kept as members of the class.
    def splitStats(stats: List[Tree]): Unit = stats match {
      case stat :: stats1 =>
        stat match {
          case stat @ ValDef(mods, name, tpt, rhs) if !stat.symbol.is(Lazy) =>
            val sym = stat.symbol
            if (isRetained(sym)) {
              if (!rhs.isEmpty && !isWildcardArg(rhs))
                constrStats += assign(sym, intoConstr(rhs)).withPos(stat.pos)
              clsStats += cpy.ValDef(stat)(rhs = EmptyTree)
            }
            else if (!rhs.isEmpty) {
              sym.copySymDenotation(
                initFlags = sym.flags &~ Private,
                owner = constr.symbol).installAfter(thisTransform)
              constrStats += intoConstr(stat)
            }
          case _: DefTree =>
            clsStats += stat
          case _ =>
            constrStats += intoConstr(stat)
        }
        splitStats(stats1)
      case Nil =>
        (Nil, Nil)
    }
    splitStats(tree.body)

    val accessorFields = accessors.filterNot(_ is Method)

    // The initializers for the retained accessors */
    val copyParams = accessorFields.filter(isRetained).map(acc =>
      assign(acc, ref(acc.subst(accessors, paramSyms))).withPos(tree.pos))

    // Drop accessors that are not retained from class scope
    val dropped = usage.dropped
    if (dropped.nonEmpty) {
      val clsInfo = cls.classInfo // TODO investigate: expand clsInfo to cls.info => dotty type error
      cls.copy(
        info = clsInfo.derivedClassInfo(
          decls = clsInfo.decls.filteredScope(!dropped.contains(_))))
    }

    cpy.Template(tree)(
      constr = cpy.DefDef(constr)(
        rhs = Block(superCalls.toList ::: copyParams ::: constrStats.toList, unitLiteral)),
      parents = parentTypeTrees,
      body = clsStats.toList)
  }
}