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
import collection.mutable

/** This transform
 *   - moves initializers from body to constructor.
 *   - makes all supercalls explicit
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
      (owner.isLocalDummy || owner.isTerm && !owner.is(Method)) &&
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

  /** Adjustments performed when moving code into the constructor:
   *  (1) Replace references to param accessors by constructor parameters
   *      except possibly references to mutable variables, if `excluded = Mutable`.
   *      (Mutable parameters should be replaced only during the super call)
   *  (2) If the parameter accessor reference was to an alias getter,
   *      drop the () when replacing by the parameter.
   */
  class IntoConstrMap(accessors: List[Symbol], params: List[Symbol]) extends TreeMap {
    private var excluded: FlagSet = _
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Ident(_) | Select(This(_), _) =>
        val sym = tree.symbol
        if (sym is (ParamAccessor, butNot = excluded)) {
          val param = sym.subst(accessors, params)
          if (param ne sym) ref(param).withPos(tree.pos)
          else tree
        }
        else tree
      case Apply(fn, Nil) =>
        val fn1 = transform(fn)
        if ((fn1 ne fn) && fn1.symbol.is(Param) && fn1.symbol.owner.isPrimaryConstructor)
          fn1 // in this case, fn1.symbol was an alias for a parameter in a superclass
        else cpy.Apply(tree)(fn1, Nil)
      case _ =>
        if (noDirectRefsFrom(tree)) tree else super.transform(tree)
    }

    def apply(tree: Tree, excluded: FlagSet)(implicit ctx: Context): Tree = {
      this.excluded = excluded
      transform(tree)
    }
  }

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
        i"lengths differ for $cls, param accs = $accessors, params = $vparamsWithOuter")
    }
    val paramSyms = vparamsWithOuter map (_.symbol)

    val intoConstr = new IntoConstrMap(accessors, paramSyms)
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
        val mappedArgs = superArgs.map(
          intoConstr(_, excluded = if (toClass) Mutable else EmptyFlags))
        val receiver =
          if (toClass) Super(This(cls), tpnme.EMPTY, inConstrCall = true)
          else This(cls)
        superCalls += cpy.Apply(superApp)(
            receiver.withPos(superNew.pos)
              .select(superSel.symbol).withPos(superSel.pos),
            mappedArgs)
        superType
      case tree: TypeTree => tree
    }
    val parentTypeTrees = tree.parents.map(normalizeParent)

    // Split class body into statements that go into constructor and
    // definitions that are kept as members of the class.
    def splitStats(stats: List[Tree]): (List[Tree], List[Tree]) = stats match {
      case stat :: stats1 =>
        val (constrStats, clsStats) = splitStats(stats1)
        stat match {
          case stat @ ValDef(mods, name, tpt, rhs) =>
            val inits =
              if (rhs.isEmpty || isWildcardArg(rhs)) Nil
              else Assign(ref(stat.symbol), intoConstr(rhs, excluded = Mutable))
                .withPos(stat.pos) :: Nil
            (inits ::: constrStats, cpy.ValDef(stat)(rhs = EmptyTree) :: clsStats)
          case _: DefTree =>
            (constrStats, stat :: clsStats)
          case _ =>
            (intoConstr(stat, excluded = Mutable) :: constrStats, clsStats)
        }
      case Nil =>
        (Nil, Nil)
    }
    val (constrStats, clsStats) = splitStats(tree.body)

    // Collect all private parameter accessors that need to be retained
    // because they are accessed after the constructor has finished.
    val collectRetained = new TreeAccumulator[Set[Symbol]] {
      override def apply(retained: Set[Symbol], tree: Tree) = tree match {
        case tree: RefTree =>
          val sym = tree.symbol
          foldOver(
            if (sym.is(PrivateParamAccessor) && sym.owner == cls) retained + sym else retained,
            tree)
        case _ =>
          if (noDirectRefsFrom(tree)) retained else foldOver(retained, tree)
      }
    }
    val retainedPrivate = collectRetained(collectRetained(Set[Symbol](), constrStats), clsStats)
    def isRetained(acc: Symbol) =
      (!acc.is(Private) || acc.is(NotJavaPrivate) || retainedPrivate(acc))

    val accessorFields = accessors.filterNot(_ is Method)
    val (retainedAccessors, droppedAccessors) = accessorFields.partition(isRetained)

    // The initializers for the retained accessors */
    val copyParams = retainedAccessors.map(acc =>
      Assign(ref(acc), ref(acc.subst(accessors, paramSyms))).withPos(tree.pos))

    // Drop accessors that are not retained from class scope
    if (droppedAccessors.nonEmpty) {
      val clsInfo = cls.classInfo // TODO investigate: expand clsInfo to cls.info => dotty type error
      cls.copy(
        info = clsInfo.derivedClassInfo(
          decls = clsInfo.decls.filteredScope(!droppedAccessors.contains(_))))
    }

    cpy.Template(tree)(
      constr = cpy.DefDef(constr)(
        rhs = Block(superCalls.toList ::: copyParams ::: constrStats, unitLiteral)),
      parents = parentTypeTrees,
      body = clsStats filter {
        case vdef: ValDef => !droppedAccessors.contains(vdef.symbol)
        case _ => true
      })
  }
}