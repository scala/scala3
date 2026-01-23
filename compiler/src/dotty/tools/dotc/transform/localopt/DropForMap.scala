package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.desugar.{TrailingForMap, TuplingMigrationForMap}
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

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

  import DropForMap.{Converted, Unmapped}

  /** r.map(x => x)(using y) --> r
   *       ^ TrailingForMap
   */
  override def transformApply(tree: Apply)(using Context): Tree =
    tree.removeAttachment(TuplingMigrationForMap).foreach: _ =>
      def stripApplies(tree: Tree): Tree = tree match
        case Apply(fun, _)     => stripApplies(fun)
        case TypeApply(fun, _) => stripApplies(fun)
        case tree => tree
      stripApplies(tree) match
      case Select(coll, _) =>
        val m = coll.tpe.nonPrivateMember(nme.map)
        if m.isOverloaded then
          val alts = m.alternatives
          val head = alts.head.info.finalResultType
          if !alts.tail.forall(_.info.finalResultType =:= head) then
            val msg = em"For comprehension with multiple val assignments may change result type in 3.8"
            report.warning(msg, tree.srcPos)
      case _ =>
    tree match
    case Unmapped(f0, sym, args) =>
      val f =
        if sym.is(Extension) then args.head
        else f0
      if f.tpe.widen =:= tree.tpe then // make sure that the type of the expression won't change
        f // drop the map call
      else
        f match
        case Converted(r) if r.tpe =:= tree.tpe => r // drop the map call and the conversion
        case _ => tree
    case tree => tree

  /** If the map was inlined, fetch the binding for the receiver,
   *  then find the tree in the expansion that refers to the binding.
   *  That is the expansion of the result Inlined node.
   */
  override def transformInlined(tree: Inlined)(using Context): Tree = tree match
    case Inlined(call, bindings, expansion) if call.hasAttachment(TrailingForMap) =>
      val expansion1 =
        call match
        case Unmapped(f0, sym, args) =>
          val f =
            if sym.is(Extension) then args.head
            else f0
          if f.tpe.widen =:= expansion.tpe then
            bindings.collectFirst:
              case vd: ValDef if f.sameTree(vd.rhs) =>
                expansion.find:
                  case Inlined(Thicket(Nil), Nil, id @ Ident(vd.name)) => id.symbol eq vd.symbol
                  case _ => false
                .getOrElse(expansion)
            .getOrElse(expansion)
          else
            f match
            case Converted(r) if r.tpe =:= expansion.tpe => r // drop the map call and the conversion
            case _ => expansion
        case _ => expansion
      if expansion1 ne expansion then
        cpy.Inlined(tree)(call, bindings, expansion1)
      else tree
    case tree => tree

object DropForMap:
  val name: String = "dropForMap"
  val description: String = "drop unused trailing map calls in for comprehensions"

  /** If the tree represents the trailing or terminal `map` of a for comprehension,
   *  extracts a fun from a possibly nested Apply with lambda and arbitrary implicit args.
   *  Specifically, an application `r.map(x => x)` is destructured into (r, map, args).
   *  If the receiver r was adapted, it is unwrapped.
   *  If `map` is an extension method, the nominal receiver is `args.head`.
   */
  private object Unmapped:
    private def loop(tree: Tree)(using Context): Option[(Tree, Symbol, List[Tree])] = tree match
      case Apply(fun, args @ Lambda(_ :: Nil, _) :: Nil) =>
        tree.removeAttachment(TrailingForMap) match
        case Some(_) =>
          fun match
          case MapCall(f, sym, args) => Some((f, sym, args))
          case _ => None
        case _ => None
      case Apply(fun, _) =>
        fun.tpe match
        case mt: MethodType if mt.isImplicitMethod => loop(fun)
        case _ => None
      case TypeApply(fun, _) => loop(fun)
      case _ => None
    end loop
    def unapply(tree: Apply)(using Context): Option[(Tree, Symbol, List[Tree])] =
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
    def unapply(tree: Tree)(using Context): Option[(Tree, Symbol, List[Tree])] =
      def loop(tree: Tree, args: List[Tree]): Option[(Tree, Symbol, List[Tree])] =
        tree match
        case Ident(nme.map) if tree.symbol.is(Extension) => Some((EmptyTree, tree.symbol, args))
        case Select(f, nme.map) => Some((f, tree.symbol, args))
        case Apply(fn, args) => loop(fn, args)
        case TypeApply(fn, _) => loop(fn, args)
        case _ => None
      loop(tree, Nil)

  private object Converted:
    def unapply(tree: Tree)(using Context): Option[Tree] = tree match
      case Apply(fn @ Apply(_, _), _) => unapply(fn)
      case Apply(fn, r :: Nil)
      if fn.symbol.is(Implicit)
      || fn.symbol.name == nme.apply && fn.symbol.owner.derivesFrom(defn.ConversionClass)
      => Some(r)
      case TypeApply(fn, _) => unapply(fn)
      case _ => None
