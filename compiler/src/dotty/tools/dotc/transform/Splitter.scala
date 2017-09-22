package dotty.tools.dotc
package transform

import TreeTransforms._
import ast.Trees._
import core._
import Contexts._, Types._, Decorators._, Denotations._, Symbols._, SymDenotations._, Names._

/** Distribute applications into Block and If nodes
 */
class Splitter extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "splitter"

  /** Distribute arguments among splitted branches */
  def distribute(tree: GenericApply[Type], rebuild: (Tree, List[Tree]) => Context => Tree)(implicit ctx: Context) = {
    def recur(fn: Tree): Tree = fn match {
      case Block(stats, expr) => Block(stats, recur(expr))
      case If(cond, thenp, elsep) => If(cond, recur(thenp), recur(elsep))
      case _ => rebuild(fn, tree.args)(ctx) withPos tree.pos
    }
    recur(tree.fun)
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo) =
    distribute(tree, typeApply)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
    distribute(tree, apply)

  private val typeApply = (fn: Tree, args: List[Tree]) => (ctx: Context) => TypeApply(fn, args)(ctx)
  private val apply     = (fn: Tree, args: List[Tree]) => (ctx: Context) => Apply(fn, args)(ctx)

/* The following is no longer necessary, since we select members on the join of an or type:
 *
  /** If we select a name, make sure the node has a symbol.
   *  If necessary, split the qualifier with type tests.
   *  Example: Assume:
   *
   *      class A { def f(x: S): T }
   *      class B { def f(x: S): T }
   *      def p(): A | B
   *
   *  Then   p().f(a)   translates to
   *
   *      val ev$1 = p()
   *      if (ev$1.isInstanceOf[A]) ev$1.asInstanceOf[A].f(a)
   *      else ev$1.asInstanceOf[B].f(a)
   */
  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) = {
    val Select(qual, name) = tree

    def memberDenot(tp: Type): SingleDenotation = {
      val mbr = tp.member(name)
      if (!mbr.isOverloaded) mbr.asSingleDenotation
      else tree.tpe match {
        case tref: TermRef if !tref.signature.isOverloaded =>
          mbr.atSignature(tref.sig).checkUnique
        case _ =>
          def alts = mbr.alternatives.map(alt => i"$alt: ${alt.info}").mkString(", ")
          ctx.error(s"cannot disambiguate overloaded members $alts", tree.pos)
          NoDenotation
      }
    }

    def candidates(tp: Type): List[Symbol] = {
      val mbr = memberDenot(tp)
      if (mbr.symbol.exists) mbr.symbol :: Nil
      else tp.widen match {
        case tref: TypeRef =>
          tref.info match {
            case TypeBounds(_, hi) => candidates(hi)
            case _ => Nil
          }
        case OrType(tp1, tp2) =>
          candidates(tp1) | candidates(tp2)
        case AndType(tp1, tp2) =>
          candidates(tp1) & candidates(tp2)
        case tpw =>
          Nil
      }
    }

    def isStructuralSelect(tp: Type): Boolean = tp.stripTypeVar match {
      case tp: RefinedType => tp.refinedName == name || isStructuralSelect(tp.parent)
      case tp: TypeProxy => isStructuralSelect(tp.underlying)
      case AndType(tp1, tp2) => isStructuralSelect(tp1) || isStructuralSelect(tp2)
      case _ => false
    }

    if (tree.symbol.exists) tree
    else {
      def choose(qual: Tree, syms: List[Symbol]): Tree = {
        def testOrCast(which: Symbol, mbr: Symbol) =
          qual.select(which).appliedToType(mbr.owner.appliedRef)
        def select(sym: Symbol) = {
          val qual1 =
            if (qual.tpe derivesFrom sym.owner) qual
            else testOrCast(defn.Any_asInstanceOf, sym)
          qual1.select(sym).withPos(tree.pos)
        }
        syms match {
          case Nil =>
            def msg =
              if (isStructuralSelect(qual.tpe))
                s"cannot access member '$name' from structural type ${qual.tpe.widen.show}; use Dynamic instead"
              else
                s"no candidate symbols for ${tree.tpe.show} found in ${qual.tpe.show}"
            ctx.error(msg, tree.pos)
            tree
          case sym :: Nil =>
            select(sym)
          case sym :: syms1 =>
            If(testOrCast(defn.Any_isInstanceOf, sym), select(sym), choose(qual, syms1))
        }
      }
      evalOnce(qual)(qual => choose(qual, candidates(qual.tpe)))
    }
  }
*/
}
