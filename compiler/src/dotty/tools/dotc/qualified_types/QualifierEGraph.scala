package dotty.tools.dotc.qualified_types

import scala.collection.mutable

import dotty.tools.dotc.ast.tpd.{
  Apply,
  ConstantTree,
  Ident,
  Literal,
  New,
  Select,
  Tree,
  TreeMap,
  TreeOps,
  TypeApply,
  TypeTree
}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{ConstantType, NoPrefix, SingletonType, TermRef, Type}
import dotty.tools.dotc.transform.TreeExtractors.BinaryOp
import dotty.tools.dotc.util.Spans.Span

private enum ENode:
  case Const(value: Constant)
  case Ref(tp: TermRef)
  case Object(clazz: Symbol, args: List[ENode])
  case Select(qual: ENode, member: Symbol)
  case App(fn: ENode, args: List[ENode])
  case TypeApp(fn: ENode, args: List[Type])

  override def toString(): String =
    this match
      case Const(value)         => value.toString
      case Ref(tp)              => termRefToString(tp)
      case Object(clazz, args)  => s"#$clazz(${args.mkString(", ")})"
      case Select(qual, member) => s"$qual..$member"
      case App(fn, args)        => s"$fn(${args.mkString(", ")})"
      case TypeApp(fn, args)    => s"$fn[${args.mkString(", ")}]"

  private def designatorToString(d: Designator): String =
    d match
      case d: Symbol => d.lastKnownDenotation.name.toString
      case _         => d.toString

  private def termRefToString(tp: Type): String =
    tp match
      case tp: TermRef =>
        val pre = if tp.prefix == NoPrefix then "" else termRefToString(tp.prefix) + "."
        pre + designatorToString(tp.designator)
      case _ =>
        tp.toString

final class QualifierEGraph:
  private val represententOf = mutable.Map.empty[ENode, ENode]

  private def representent(node: ENode): ENode =
    represententOf.get(node) match
      case None => node
      case Some(repr) =>
        val res = representent(repr) // avoid tailrec optimization
        res

  /** Map from child nodes to their parent nodes */
  private val usedBy = mutable.Map.empty[ENode, mutable.Set[ENode]]

  private def uses(node: ENode): mutable.Set[ENode] =
    usedBy.getOrElseUpdate(node, mutable.Set.empty)

  /** Map used for hash-consing nodes, keys and values are the same */
  private val index = mutable.Map.empty[ENode, ENode]

  private val worklist = mutable.Queue.empty[ENode]

  final def union(tree1: Tree, tree2: Tree)(using Context): Unit =
    for node1 <- toNode(tree1); node2 <- toNode(tree2) do
      merge(node1, node2)

  private def unique(node: ENode): node.type =
    index.getOrElseUpdate(
      node, {
        node match
          case ENode.Const(value) =>
            ()
          case ENode.Ref(tp) =>
            ()
          case ENode.Object(clazz, args) =>
            args.foreach(uses(_) += node)
          case ENode.Select(qual, member) =>
            uses(qual) += node
          case ENode.App(fn, args) =>
            uses(fn) += node
            args.foreach(uses(_) += node)
          case ENode.TypeApp(fn, args) =>
            uses(fn) += node
        node
      }
    ).asInstanceOf[node.type]

  private val toNodeCache = mutable.WeakHashMap.empty[Tree, Option[ENode]]

  private def toNode(tree: Tree)(using Context): Option[ENode] =
    toNodeCache.getOrElseUpdate(tree, computeToNode(tree).map(n => representent(unique(n))))

  private def computeToNode(tree: Tree)(using Context): Option[ENode] =
    tree match
      case ConstantTree(constant) =>
        Some(ENode.Const(constant))
      case Ident(_) =>
        tree.tpe match
          case tp: TermRef => Some(ENode.Ref(tp))
          case _           => None
      case Apply(Select(clazz, nme.CONSTRUCTOR), args) if isCaseClass(clazz.symbol) =>
        for argsNodes <- args.map(toNode).sequence yield ENode.Object(clazz.symbol, argsNodes)
      case Select(qual, name) if isCaseClassField(tree.symbol) =>
        for qualNode <- toNode(qual) yield qualNode match
          case ENode.Object(_, args) => args(caseClassFieldIndex(tree.symbol))
          case qualNode              => ENode.Select(qualNode, tree.symbol)
      case Apply(fun, args) =>
        for funNode <- toNode(fun); argsNodes <- args.map(toNode).sequence yield ENode.App(funNode, argsNodes)
      case TypeApply(fun, args) =>
        for funNode <- toNode(fun) yield ENode.TypeApp(funNode, args.map(_.tpe))
      case _ =>
        return None

  private object RefTypeTree:
    def unapply(tree: Tree): Option[TermRef] =
      tree.tpe match
        case tp: TermRef => Some(tp)
        case _           => None

  private def isCaseClass(sym: Symbol): Boolean =
    // TODO(mbovel)
    false

  private def isCaseClassField(sym: Symbol): Boolean =
    // TODO(mbovel)
    false

  private def caseClassFieldIndex(sym: Symbol): Int =
    // TODO(mbovel)
    ???

  private def canonicalize(node: ENode): ENode =
    representent(unique(
      node match
        case ENode.Const(value) =>
          node
        case ENode.Ref(tp) =>
          node
        case ENode.Object(clazz, args) =>
          val argsNodes = args.map(representent)
          ENode.Object(clazz, argsNodes)
        case ENode.Select(qual, member) =>
          representent(qual) match
            case ENode.Object(_, args) =>
              args(caseClassFieldIndex(member))
            case qualRepr =>
              ENode.Select(qualRepr, member)
        case ENode.App(fn, args) =>
          val fnNode = representent(fn)
          val argsNodes = args.map(representent)
          ENode.App(fnNode, argsNodes)
        case ENode.TypeApp(fn, args) =>
          val fnNode = representent(fn)
          ENode.TypeApp(fnNode, args)
    ))

  private def order(a: ENode, b: ENode): (ENode, ENode) =
    (a, b) match
      case (_: ENode.Const, _)  => (a, b)
      case (_, _: ENode.Const)  => (b, a)
      case (_: ENode.Ref, _)    => (a, b)
      case (_, _: ENode.Ref)    => (b, a)
      case (_: ENode.Object, _) => (a, b)
      case (_, _: ENode.Object) => (b, a)
      case (_: ENode.Select, _) => (a, b)
      case (_, _: ENode.Select) => (b, a)
      case (_: ENode.App, _)    => (a, b)
      case (_, _: ENode.App)    => (b, a)
      case _                    => (a, b)

  private def merge(a: ENode, b: ENode): Unit =
    val aRepr = representent(a)
    val bRepr = representent(b)
    if aRepr eq bRepr then return

    // If both nodes are objects, recursively merge their arguments
    (aRepr, bRepr) match
      case (ENode.Object(clazzA, argsA), ENode.Object(clazzB, argsB)) if clazzA == clazzB =>
        argsA.zip(argsB).foreach(merge)
      case _ => ()

    /// Update represententOf and usedBy maps
    val (newRepr, oldRepr) = order(aRepr, bRepr)
    represententOf(oldRepr) = newRepr
    uses(newRepr) ++= uses(oldRepr)
    val oldUses = uses(oldRepr)
    usedBy.remove(oldRepr)

    // Enqueue all nodes that use the oldRepr for repair
    worklist.enqueueAll(oldUses)

  def repair(): Unit =
    while !worklist.isEmpty do
      val head = worklist.dequeue()
      val headRepr = representent(head)
      val headCanonical = canonicalize(head)
      if headRepr ne headCanonical then
        merge(headRepr, headCanonical)

  // Rewrite equivalent nodes in the tree to their canonical form
  def rewrite(tree: Tree)(using Context): Tree =
    Rewriter().transform(tree)

  private class Rewriter extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      toNode(tree) match
        case Some(n) => toTree(representent(n))
        case None =>
          val d = defn
          tree match
            case BinaryOp(a, d.Int_== | d.Any_== | d.Boolean_==, b) =>
              (toNode(a), toNode(b)) match
                case (Some(aNode), Some(bNode)) =>
                  if representent(aNode) eq representent(bNode) then Literal(Constant(true))
                  else super.transform(tree)
                case _ =>
                  super.transform(tree)
            case _ =>
              super.transform(tree)

  private def toTree(node: ENode)(using Context): Tree =
    node match
      case ENode.Const(value) =>
        Literal(value)
      case ENode.Ref(tp) =>
        Ident(tp)
      case ENode.Object(clazz, args) =>
        New(clazz.typeRef, args.map(toTree))
      case ENode.Select(qual, member) =>
        toTree(qual).select(member)
      case ENode.App(fn, args) =>
        Apply(toTree(fn), args.map(toTree))
      case ENode.TypeApp(fn, args) =>
        TypeApply(toTree(fn), args.map(TypeTree(_, false)))

  extension [T](xs: List[Option[T]])
    def sequence: Option[List[T]] =
      var result = List.newBuilder[T]
      var current = xs
      while current.nonEmpty do
        current.head match
          case Some(x) =>
            result += x
            current = current.tail
          case None =>
            return None
      Some(result.result())
