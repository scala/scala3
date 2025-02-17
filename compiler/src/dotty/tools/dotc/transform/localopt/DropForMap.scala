package dotty.tools.dotc
package transform.localopt

import scala.language.unsafeNulls

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ConstFold
import dotty.tools.dotc.ast.desugar
import scala.util.chaining.*
import tpd.*

class DropForMap extends MiniPhase:
  import DropForMap.*
  import Binder.*

  override def phaseName: String = DropForMap.name

  override def description: String = DropForMap.description

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    if !tree.hasAttachment(desugar.TrailingForMap) then tree
    else tree match
      case aply @ Apply(MapCall(f), List(Lambda(List(param), body)))
      if canDropMap(Single(param), body) && f.tpe =:= aply.tpe => // make sure that the type of the expression won't change
        f // drop the map call
      case _ =>
        tree.tap(_.removeAttachment(desugar.TrailingForMap))

  private object Lambda:
    def unapply(tree: Tree)(using Context): Option[(List[ValDef], Tree)] =
      tree match
        case Block(List(defdef: DefDef), Closure(Nil, ref, _)) if ref.symbol == defdef.symbol && !defdef.paramss.exists(_.forall(_.isType)) =>
          Some((defdef.termParamss.flatten, defdef.rhs))
        case _ => None

  private object MapCall:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Select(f, nme.map) => Some(f)
      case Apply(fn, _) => unapply(fn)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None

  /** We can drop the map call if:
    * - it is a Unit literal
    * - is an identity function (i.e. the last pattern is the same as the result)
    */
  private def canDropMap(params: Binder, tree: Tree)(using Context): Boolean = tree match
    case Literal(Constant(())) => params match
      case Single(bind) => bind.symbol.info.isRef(defn.UnitClass)
      case _ => false
    case ident: Ident => params match
      case Single(bind) => bind.symbol == ident.symbol
      case _ => false
    case tree: Apply if tree.tpe.typeSymbol.derivesFrom(defn.TupleClass) => params match
      case Tuple(binds) => tree.args.zip(binds).forall((arg, param) => canDropMap(param, arg))
      case _ => false
    case Match(scrutinee, List(CaseDef(pat, EmptyTree, body))) =>
      val newParams = newParamsFromMatch(params, scrutinee, pat)
      canDropMap(newParams, body)
    case Block(Nil, expr) => canDropMap(params, expr)
    case _ =>
      false

  /** Extract potentially new parameters from a match expression
    */
  private def newParamsFromMatch(params: Binder, scrutinee: Tree, pat: Tree)(using Context): Binder =
    def extractTraverse(pats: List[Tree]): Option[List[Binder]] = pats match
      case Nil => Some(List.empty)
      case pat :: pats =>
        extractBinders(pat).map(_ +: extractTraverse(pats).get)
    def extractBinders(pat: Tree): Option[Binder] = pat match
      case bind: Bind => Some(Single(bind))
      case tree @ UnApply(fun, implicits, pats)
        if implicits.isEmpty && tree.tpe.finalResultType.dealias.typeSymbol.derivesFrom(defn.TupleClass) =>
          extractTraverse(pats).map(Tuple.apply)
      case _ => None

    params match
      case Single(bind) if scrutinee.symbol == bind.symbol =>
        pat match
          case bind: Bind => Single(bind)
          case tree @ UnApply(fun, implicits, pats) if implicits.isEmpty =>
            val unapplied = tree.tpe.finalResultType.dealias.typeSymbol
            if unapplied.derivesFrom(defn.TupleClass) then
              extractTraverse(pats).map(Tuple.apply).getOrElse(params)
            else params
          case _ => params
      case _ => params

object DropForMap:
  val name: String = "dropForMap"
  val description: String = "Drop unused trailing map calls in for comprehensions"

  private enum Binder:
    case Single(bind: NamedDefTree)
    case Tuple(binds: List[Binder])
