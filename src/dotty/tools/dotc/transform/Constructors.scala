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
import ExplicitOuter.outerParamAccessor

/** This transform moves initializers from body to constructor.
 */
class Constructors extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import tpd._

  override def phaseName: String = "constructors"
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  override def treeTransformPhase = thisTransform.next

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def ownerBecomesConstructor(owner: Symbol): Boolean =
      (owner.isLocalDummy ||
       owner.isTerm && !owner.is(Method) && owner.owner.isClass) &&
        !owner.enclosingClass.is(Trait)   // TODO: Remove qualification once Mixin is operational
    if (ownerBecomesConstructor(sym.owner))
      sym.copySymDenotation(owner = sym.owner.enclosingClass.primaryConstructor)
    else sym
  }

  private def intoConstr(accessors: List[Symbol], params: List[Symbol]) = new TreeMap {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Ident(_) | Select(This(_), _) =>
        val sym = tree.symbol
        if (sym is ParamAccessor) {
          val param = sym.subst(accessors, params)
          if (param ne sym) ref(param).withPos(tree.pos)
          else tree
        }
        else tree
      case Apply(fn, Nil) =>
        val fn1 = transform(fn)
        if ((fn1 ne fn) &&
            fn1.symbol.is(Param) &&
            fn1.symbol.owner.isPrimaryConstructor) {
          // Two possible cases, which each need their adaptation:
          if (fn1.symbol.initial.info.isInstanceOf[ExprType])
            // it's either a call-by-name parameter, which is erased to Function0,
            // then we need to insert an apply.
            cpy.Apply(tree)(Select(fn1, nme.apply), Nil).ensureConforms(tree.tpe)
          else
            // or original accessor was an alias accessor, then we need to drop the ()
            fn1
        }
        else cpy.Apply(tree)(fn1, Nil)
      case _ =>
        super.transform(tree)
    }
  }

  private def splitStats(stats: List[Tree])(implicit ctx: Context): (List[Tree], List[Tree]) = stats match {
    case stat :: stats1 =>
      val (constrStats, clsStats) = splitStats(stats1)
      stat match {
        case stat @ ValDef(mods, name, tpt, rhs) if !rhs.isEmpty =>
          val inits =
            if (isWildcardArg(rhs)) Nil
            else Assign(ref(stat.symbol), rhs).withPos(stat.pos) :: Nil
          (inits ::: constrStats, cpy.ValDef(stat)(rhs = EmptyTree) :: clsStats)
        case _: DefTree =>
          (constrStats, stat :: clsStats)
        case _ =>
          (stat :: constrStats, clsStats)
      }
    case Nil =>
      (Nil, Nil)
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass
    if (cls is Trait) tree
    else {
      val constr @ DefDef(_, nme.CONSTRUCTOR, Nil, vparams :: Nil, _, EmptyTree) = tree.constr
      val (superApp @ Apply(
             superSel @ Select(
               superNew @ New(superType),
               nme.CONSTRUCTOR),
             superArgs)) :: traitParents = tree.parents
      var accessors = cls.paramAccessors.filterNot(_.isSetter)
      var vparamsWithOuter = vparams
      if (!accessors.hasSameLengthAs(vparams)) {
        accessors.reverse match {
          case last :: _ if (last.name == nme.OUTER) =>
            accessors = last :: accessors.init
            vparamsWithOuter = ValDef(last.asTerm) :: vparams
          case _ =>
        }
        assert(accessors.hasSameLengthAs(vparamsWithOuter),
          i"lengths differ for $cls, param accs = $accessors, params = $vparamsWithOuter")
      }
      val mappedArgs = superArgs.map(
        intoConstr(accessors, vparamsWithOuter.map(_.symbol)).transform)
      val superCall =
        cpy.Apply(superApp)(
          cpy.Select(superSel)(
            Super(This(cls), tpnme.EMPTY, inConstrCall = true).withPos(superNew.pos),
            nme.CONSTRUCTOR),
          mappedArgs)
      val (constrStats, clsStats) = splitStats(tree.body)
      def normalizeOwner(stat: Tree) = {
      }
      cpy.Template(tree)(
        constr = cpy.DefDef(constr)(rhs = Block(superCall :: constrStats, unitLiteral)),
        parents = superType :: traitParents,
        body = clsStats)
    }
  }
}