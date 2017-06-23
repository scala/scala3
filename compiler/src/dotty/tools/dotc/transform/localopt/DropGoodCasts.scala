package dotty.tools.dotc
package transform.localopt

import core._
import core.Constants.Constant
import core.Contexts.Context
import core.Decorators._
import core.Symbols._
import core.Types._
import core.Flags._
import ast.Trees._
import transform.SymUtils._
import Simplify.isEffectivelyMutable

/** Eliminated casts and equality tests whose results can be locally
 *  determined at compile time:
 *
 *  - a.asInstanceOf[T] → a when we know that a: T
 *  - Simplify (a == null) and (a != null) when the result is statically known
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
 class DropGoodCasts extends Optimisation {
  import ast.tpd._

  def visitor(implicit ctx: Context) = NoVisitor
  def clear(): Unit = ()

  def transformer(implicit ctx: Context): Tree => Tree = {
    case t @ If(cond, thenp, elsep) =>
      val newTypeTested = collectTypeTests(cond)
      val nullTested = collectNullTests(cond).toSet
      val testedMap = newTypeTested.foldRight[Map[Symbol, List[Type]]](Map.empty) { case (x, y) =>
        y + ((x._1, x._2 :: y.getOrElse(x._1, Nil)))
      }
      val dropGoodCastsInStats = new TreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = {
          def applyCondition(fun: Select, tree: Tree, const: Constant): Boolean =
            const.tag == Constants.NullTag &&
            (fun.symbol == defn.Object_eq || fun.symbol == defn.Object_ne) &&
            (nullTested.contains(tree.symbol))

          def applyBody(fun: Select): Tree =
            if (fun.symbol == defn.Object_eq) Literal(Constant(false))
            else Literal(Constant(true))

          super.transform(tree) match {
            case t: Block =>
              val nstats = t.stats.filterConserve({
                case TypeApply(fun @ Select(rec, _), List(tp))
                  if fun.symbol == defn.Any_asInstanceOf =>
                    !testedMap.getOrElse(rec.symbol, Nil).exists(x => x <:< tp.tpe)
                case _ => true
              })
              if (nstats eq t.stats) t
              else Block(nstats, t.expr)
            case Apply(fun @ Select(lhs, _), List(Literal(const))) if applyCondition(fun, lhs, const) =>
              applyBody(fun)
            case Apply(fun @ Select(Literal(const), _), List(rhs)) if applyCondition(fun, rhs, const) =>
              applyBody(fun)
            case t => t
          }
        }
      }
      val nthenp = dropGoodCastsInStats.transform(thenp)

      cpy.If(t)(thenp = nthenp, elsep = elsep)
    case t => t
  }

  def collectTypeTests(t: Tree)(implicit ctx: Context): List[(Symbol, Type)] = {
    def recur(t: Tree): List[(Symbol, Type)] =
      t match {
        case Apply(x, _) if (x.symbol == defn.Boolean_! || x.symbol == defn.Boolean_||) =>
          Nil

        case Apply(fun @ Select(x, _), y) if (fun.symbol == defn.Boolean_&&) =>
          recur(x) ++ recur(y.head)

        case TypeApply(fun @ Select(x, _), List(tp))
          if fun.symbol.eq(defn.Any_isInstanceOf) &&
             !isEffectivelyMutable(x)             &&
             !x.symbol.is(Method)                 &&
             x.symbol.exists && !x.symbol.owner.isClass =>
          (x.symbol, tp.tpe) :: Nil

        case _ => Nil
      }
    recur(t)
  }

  def collectNullTests(t: Tree)(implicit ctx: Context): List[Symbol] = {
    def recur(t: Tree): List[Symbol] =
      t match {
        case Apply(x, _) if (x.symbol == defn.Boolean_! || x.symbol == defn.Boolean_||) =>
          Nil

        case Apply(fun @ Select(x, _), y) if (fun.symbol == defn.Boolean_&&) =>
          recur(x) ++ recur(y.head)

        case Apply(fun @ Select(x, _), List(tp))
            if fun.symbol.eq(defn.Object_ne)  &&
               !isEffectivelyMutable(x)       &&
               !x.symbol.is(Method)           &&
               x.symbol.exists && !x.symbol.owner.isClass =>
          x.symbol :: Nil

        case _ => Nil
      }
    recur(t)
  }
}
