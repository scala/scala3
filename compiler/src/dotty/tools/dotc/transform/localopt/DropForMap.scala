package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.ast.desugar

/** Drop unused trailing map calls in for comprehensions.
 *
 *  We can drop the map call if:
 *  - it won't change the type of the expression, and
 *  - the function is an identity function or a const function to unit.
 *
 *  The latter condition is checked in [[Desugar.scala#makeFor]]
 */
class DropForMap extends MiniPhase:

  override def phaseName: String = DropForMap.name

  override def description: String = DropForMap.description

  /** r.map(x => x)(using y) --> r
   *       ^ TrailingForMap
   */
  override def transformApply(tree: Apply)(using Context): Tree = tree match
    case Unmapped(f) =>
      if f.tpe =:= tree.tpe then // make sure that the type of the expression won't change
        f // drop the map call
      else
        f match
        case Converted(r) if r.tpe =:= tree.tpe => r // drop the map call and the conversion
        case _ => tree
    case tree => tree

  override def transformInlined(tree: Inlined)(using Context): Tree = tree match
    case Inlined(call, bindings, expansion) if expansion.hasAttachment(desugar.TrailingForMap) =>
      call match
      case Unmapped(f) =>
        bindings.collectFirst:
          case vd: ValDef if f.sameTree(vd.rhs) =>
            expansion.find:
              case Inlined(Thicket(Nil), Nil, Ident(ident)) => ident == vd.name
              case _ => false
            .match
              case Some(ref) => cpy.Inlined(tree)(call, bindings, ref)
              case _ => tree
        .getOrElse(tree)
      case _ => tree
    case tree => tree

  // Extracts a fun from a possibly nested Apply with lambda and arbitrary implicit args.
  private object Unmapped:
    private def loop(tree: Tree)(using Context): Option[Tree] =
      tree match
      case Apply(fun, Lambda(_ :: Nil, _) :: Nil) =>
        tree.removeAttachment(desugar.TrailingForMap) match
        case Some(_) =>
          fun match
          case MapCall(f) => return Some(f)
          case _ =>
        case _ =>
      case Apply(fun, _) =>
        fun.tpe match
        case mt: MethodType if mt.isImplicitMethod => return loop(fun)
        case _ =>
      case _ =>
      None
    end loop
    def unapply(tree: Apply)(using Context): Option[Tree] =
      tree.tpe match
      case _: MethodOrPoly => None
      case _ => loop(tree)

  private object Lambda:
    def unapply(tree: Tree)(using Context): Option[(List[ValDef], Tree)] = tree match
      case Block(List(defdef: DefDef), Closure(Nil, ref, _))
      if ref.symbol == defdef.symbol && !defdef.paramss.exists(_.forall(_.isType)) =>
        Some((defdef.termParamss.flatten, defdef.rhs))
      case _ => None

  private object MapCall:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Select(f, nme.map) => Some(f)
      case Apply(fn, _) => unapply(fn)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None

  private object Converted:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Apply(fn @ Apply(_, _), _) => unapply(fn)
      case Apply(fn, r :: Nil)
      if fn.symbol.is(Implicit) || fn.symbol.name == nme.apply && fn.symbol.owner.derivesFrom(defn.ConversionClass)
      => Some(r)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None

object DropForMap:
  val name: String = "dropForMap"
  val description: String = "Drop unused trailing map calls in for comprehensions"
