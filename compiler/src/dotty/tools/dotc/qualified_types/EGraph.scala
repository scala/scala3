package dotty.tools.dotc.qualified_types

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.ast.tpd.{
  closureDef,
  singleton,
  Apply,
  ConstantTree,
  Ident,
  Lambda,
  Literal,
  New,
  Select,
  This,
  Tree,
  TreeMap,
  TreeOps,
  TypeApply,
  TypeTree
}
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{
  AppliedType,
  CachedConstantType,
  CachedProxyType,
  ConstantType,
  LambdaType,
  MethodType,
  NamedType,
  NoPrefix,
  SingletonType,
  SkolemType,
  TermParamRef,
  TermRef,
  Type,
  TypeRef,
  TypeVar,
  ValueType
}
import dotty.tools.dotc.core.Uniques
import dotty.tools.dotc.qualified_types.ENode.Op
import dotty.tools.dotc.transform.TreeExtractors.BinaryOp
import dotty.tools.dotc.util.{EqHashMap, HashMap}
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.reporting
import dotty.tools.dotc.config.Printers

import annotation.threadUnsafe as tu
import reflect.ClassTag

final class EGraph(_ctx: Context, checksEnabled: Boolean = true):

  /** Cache for unique E-Nodes
   *
   *  Invariant: Each key is `eq` to its associated value.
   *
   *  Invariant: If a node is in this map, then its children also are.
   */
  private val index: HashMap[ENode, ENode] = HashMap()


  private val idOf: EqHashMap[ENode, Int] = EqHashMap()

  /** Map from nodes to their unique, canonical representations.
   *
   *  Invariant: After a call to [[repair]], if a node is in the index but not
   *  in this map, then it is its own representant and it is canonical.
   *
   *  Invariant: After a call to [[repair]], values of this map are canonical.
   */
  private val representantOf: EqHashMap[ENode, ENode] = EqHashMap()

  /** Map from child nodes to their parent nodes
   *
   *  Invariant: After a call to [[repair]], values of this map are canonical.
   */
  private val usedBy: EqHashMap[ENode, mutable.Set[ENode]] = EqHashMap()

  /** Worklist for nodes that need to be repairedConstantType(Constant(value).
   *
   *  This queue is filled by [[merge]] and processed by [[repair]].
   *
   *  Invariant: After a call to [[repair]], this queue is empty.
   */
  private val worklist = mutable.Queue.empty[ENode]

  val trueNode: ENode.Atom = constant(true)
  val falseNode: ENode.Atom = constant(false)
  val zeroIntNode: ENode.Atom = constant(0)
  val minusOneIntNode: ENode.Atom = constant(-1)
  val oneIntNode: ENode.Atom = constant(1)

  /** Returns the canonical node for the given constant value */
  def constant(value: Any): ENode.Atom =
    val node = ENode.Atom(ConstantType(Constant(value))(using _ctx))
    idOf.getOrElseUpdate(node, idOf.size)
    index.getOrElseUpdate(node, node).asInstanceOf[ENode.Atom]

  /** Adds the given node to the E-Graph, returning its canonical representant.
   *
   *  Pre-condition: The node must be normalized, and its children must be
   *  canonical.
   */
  private def unique(node: ENode): ENode =
    if index.contains(node) then
      representant(index(node))
    else
      index.update(node, node)
      idOf.update(node, idOf.size)
      node match
        case ENode.Atom(tp) =>
          ()
        case ENode.Constructor(sym) =>
          ()
        case ENode.Select(qual, member) =>
          addUse(qual, node)
        case ENode.Apply(fn, args) =>
          addUse(fn, node)
          for arg <- args do
            addUse(arg, node)
        case ENode.OpApply(op, args) =>
          for arg <- args do
            addUse(arg, node)
        case ENode.TypeApply(fn, args) =>
          addUse(fn, node)
        case ENode.Lambda(paramTps, retTp, body) =>
          addUse(body, node)
          node
      node

  private def representant(node: ENode): ENode =
    representantOf.get(node) match
      case None => node
      case Some(repr) =>
        // There must be no cycles in the `representantOf` map.
        // If a node is canonical, it must have no representant.
        assert(repr ne node, s"Node $node has itself as representant ($repr)")
        representant(repr)

  def assertCanonical(node: ENode): Unit =
    if checksEnabled then
      // By the invariants, if a node is in the index (meaning it is tracked by
      // this E-Graph), and has no representant, then it is itself a canonical
      // node. We double-check by forcing a deep canonicalization.
      assert(index.contains(node) && index(node) == node, s"Node $node is not unique in this E-Graph")
      assert(!representantOf.contains(node), s"Node $node has a representant: ${representantOf(node)}")
      val canonical = canonicalize(node)
      assert(node eq canonical, s"Recanonicalization of $node did not return itself, but $canonical")

  private def addUse(child: ENode, parent: ENode): Unit =
    usedBy.getOrElseUpdate(child, mutable.Set.empty) += parent

  override def toString(): String =
    s"EGraph{\nindex = $index,\nrepresentantOf = $representantOf,\nusedBy = $usedBy,\nworklist = $worklist}\n"

  def toDot()(using Context): String =
    val sb = new StringBuilder()
    sb.append("digraph EGraph {\nnode [height=.1 shape=record]\n")
    for node <- index.valuesIterator do
      sb.append(node.toDot())
    for (node, repr) <- representantOf.iterator do
      sb.append(s"${node.dotId()} -> ${repr.dotId()} [style=dotted]\n")
    for (child, parents) <- usedBy.iterator do
      for parent <- parents do
        sb.append(s"${child.dotId()} -> ${parent.dotId()} [style=dashed]\n")
    sb.append("}\n")
    sb.toString()

  def debugString()(using _ctx: Context): String =
    given Context = _ctx.withoutColors
    index
      .valuesIterator
      .toList
      .groupBy(representant)
      .toList
      .sortBy((repr, members) => repr.showNoBreak)
      .map((repr, members) => repr.showNoBreak + ": " + members.filter(_ ne repr).map(_.showNoBreak).sorted.mkString("{", ", ", "}"))
      .mkString("", "\n", "\n")


  private inline def show(enode: ENode): String =
    enode.showNoBreak(using _ctx)

  private inline def trace[T](inline message: String)(inline f: T): T =
    reporting.trace(message, Printers.qualifiedTypes)(f)(using _ctx)

  def equiv(node1: ENode, node2: ENode): Boolean =
    trace(s"equiv ${show(node1)}, ${show(node2)}"):
      val repr1 = representant(node1)
      val repr2 = representant(node2)
      repr1 eq repr2

  def merge(a: ENode, b: ENode): Unit =
    if checksEnabled then
      assert(index.contains(a) && index(a) == a, s"Node $a is not unique in this E-Graph")
      assert(index.contains(b) && index(b) == b, s"Node $b is not unique in this E-Graph")

    val aRepr = representant(a)
    val bRepr = representant(b)

    if aRepr eq bRepr then return

    if checksEnabled then
      assert(aRepr != bRepr, s"$aRepr and $bRepr are `equals` but not `eq`")

    // Update representantOf and usedBy maps
    val (newRepr, oldRepr) = order(aRepr, bRepr)
    representantOf(oldRepr) = newRepr
    val oldusages = usedBy.getOrElse(oldRepr, mutable.Set.empty)
    usedBy.getOrElseUpdate(newRepr, mutable.Set.empty) ++= oldusages
    usedBy.remove(oldRepr)

    trace(s"merge ${show(newRepr)}  <--  ${show(oldRepr)}"):
      // Propagate truth values over disjunctions, conjunctions and equalities
      oldRepr match
        case ENode.OpApply(Op.And, args) if newRepr eq trueNode =>
          args.foreach(merge(_, trueNode))
        case ENode.OpApply(Op.Or, args) if newRepr eq falseNode =>
          args.foreach(merge(_, falseNode))
        case ENode.OpApply(Op.Equal, args) if newRepr eq trueNode =>
          merge(args(0), args(1))
        case _ =>
          ()

      // Enqueue all nodes that use the oldRepr for repair
      trace(s"enqueue ${oldusages.map(show).mkString(", ")}"):
        worklist.enqueueAll(oldusages)
        ()

  private def order(a: ENode, b: ENode): (ENode, ENode) =
    if a.contains(b) then
      (b, a)
    else if b.contains(a) then
      (a, b)
    else
      (a, b) match
        case (ENode.Atom(_: ConstantType), _) => (a, b)
        case (_, ENode.Atom(_: ConstantType)) => (b, a)
        case (_: ENode.OpApply, _) => (a, b)
        case (_, _: ENode.OpApply) => (b, a)
        case (_: ENode.Constructor, _) => (a, b)
        case (_, _: ENode.Constructor) => (b, a)
        case (_: ENode.Select, _) => (a, b)
        case (_, _: ENode.Select) => (b, a)
        case (_: ENode.Apply, _) => (a, b)
        case (_, _: ENode.Apply) => (b, a)
        case (_: ENode.TypeApply, _) => (a, b)
        case (_, _: ENode.TypeApply) => (b, a)
        case (_: ENode.Atom, _) => (a, b)
        case (_, _: ENode.Atom) => (b, a)
        case _ => (a, b)

  def repair(): Unit =
    var i = 0
    trace(s"repair (queue: ${worklist.map(show).mkString(", ")})"):
      while !worklist.isEmpty do
        val head = worklist.dequeue()
        val headRepr = representant(head)
        val headCanonical = canonicalize(head, deep = false)
        if headRepr ne headCanonical then
          trace(s"repair ${show(headCanonical)}, ${show(headRepr)}"):
            merge(headCanonical, headRepr)
        i += 1
        if i > 100 then
          throw new RuntimeException("EGraph.repair: too many iterations, possible infinite loop")

    assertInvariants()

  def assertInvariants(): Unit =
    if checksEnabled then
      assert(worklist.isEmpty, "Worklist is not empty")

      // Check that all nodes in the index are canonical
      for (node, node2) <- index.iterator do
        assert(node eq node2, s"Key and value in index are not equal: $node ne $node2")

        val repr = representant(node)
        assertCanonical(repr)

        def usages(node: ENode): mutable.Set[ENode] =
          usedBy.getOrElse(node, mutable.Set.empty)

        node match
          case ENode.Atom(tp) => ()
          case ENode.Constructor(sym) => ()
          case ENode.Select(qual, member) =>
            index.contains(qual) && usages(qual).contains(node)
          case ENode.Apply(fn, args) =>
            index.contains(fn) && usages(fn).contains(node)
            args.forall(arg => index.contains(arg) && usages(arg).contains(node))
          case ENode.OpApply(op, args) =>
            args.forall(arg => index.contains(arg) && usages(arg).contains(node))
          case ENode.TypeApply(fn, args) =>
            index.contains(fn) && usages(fn).contains(node)
          case ENode.Lambda(paramTps, retTp, body) =>
            index.contains(body) && usages(body).contains(node)

      for (node, repr) <- representantOf.iterator do
        assert(index.contains(node), s"Node $node is not in the index")

      for (child, parents) <- usedBy.iterator do
        assertCanonical(child)

  // -----------------------------------
  // Canonicalization
  // -----------------------------------

  def canonicalize(node: ENode, deep: Boolean = true): ENode =
    def recur(node: ENode): ENode =
      if deep then canonicalize(node, deep) else representant(node)
    trace(s"canonicalize ${show(node)}"):
      representant(unique(
        node match
          case ENode.Atom(tp) =>
            node
          case ENode.Constructor(sym) =>
            node
          case ENode.Select(qual, member) =>
            normalizeSelect(recur(qual), member)
          case ENode.Apply(fn, args) =>
            ENode.Apply(recur(fn), args.map(recur))
          case ENode.OpApply(op, args) =>
            normalizeOp(op, args.map(recur))
          case ENode.TypeApply(fn, args) =>
            ENode.TypeApply(recur(fn), args)
          case ENode.Lambda(paramTps, retTp, body) =>
            ENode.Lambda(paramTps, retTp, recur(body))
      ))

  private def normalizeSelect(qual: ENode, member: Symbol): ENode =
    getAppliedConstructor(qual) match
      case Some(constr) =>
        val memberIndex = constr.fields.indexOf(member)
        if memberIndex >= 0 then
          val args = getTermArguments(qual)
          assert(args.size == constr.fields.size)
          args(memberIndex)
        else
          ENode.Select(qual, member)
      case None =>
        ENode.Select(qual, member)

  private def getAppliedConstructor(node: ENode): Option[ENode.Constructor] =
    node match
      case ENode.Apply(fn, args) => getAppliedConstructor(fn)
      case ENode.TypeApply(fn, args) => getAppliedConstructor(fn)
      case node: ENode.Constructor => Some(node)
      case _ => None

  private def getTermArguments(node: ENode): List[ENode] =
    node match
      case ENode.Apply(fn, args) => getTermArguments(fn) ::: args
      case ENode.TypeApply(fn, args) => getTermArguments(fn)
      case _ => Nil

  private def normalizeOp(op: ENode.Op, args: List[ENode]): ENode =
    val res = op match
      case Op.Equal =>
        assert(args.size == 2, s"Expected 2 arguments for equality, got $args")
        if args(0) eq args(1) then
          trueNode
        else ENode.OpApply(op, args.sortBy(idOf.apply))
      case Op.And =>
        assert(args.size == 2, s"Expected 2 arguments for conjunction, got $args")
        if (args(0) eq falseNode) || (args(1) eq falseNode) then falseNode
        else if args(0) eq trueNode then args(1)
        else if args(1) eq trueNode then args(0)
        else ENode.OpApply(op, args)
      case Op.Or =>
        assert(args.size == 2, s"Expected 2 arguments for disjunction, got $args")
        if (args(0) eq trueNode) || (args(1) eq trueNode) then trueNode
        else if args(0) eq falseNode then args(1)
        else if args(1) eq falseNode then args(0)
        else ENode.OpApply(op, args)
      case Op.IntSum =>
        val (const, nonConsts) = decomposeIntSum(args)
        makeIntSum(const, nonConsts)
      case Op.IntMinus =>
        assert(args.size == 2, s"Expected 2 arguments for subtraction, got $args")
        // Rewrite a - b as a + (-1) * b
        val lhs = args(0)
        val rhs = args(1)
        val negativeRhs = unique(normalizeOp(Op.IntProduct, List(minusOneIntNode, rhs)))
        normalizeOp(Op.IntSum, List(lhs, negativeRhs))
      case Op.IntProduct =>
        val (consts, nonConsts) = decomposeIntProduct(args)
        makeIntProduct(consts, nonConsts)
      case Op.IntLessThan => constFoldBinaryOp[Int, Boolean](op, args, _ < _)
      case Op.IntLessEqual => constFoldBinaryOp[Int, Boolean](op, args, _ <= _)
      case Op.IntGreaterThan => constFoldBinaryOp[Int, Boolean](op, args, _ > _)
      case Op.IntGreaterEqual => constFoldBinaryOp[Int, Boolean](op, args, _ >= _)
      case _ =>
        ENode.OpApply(op, args)
    res

  private def constFoldBinaryOp[T: ClassTag, S](op: ENode.Op, args: List[ENode], fn: (T, T) => S): ENode =
    args match
      case List(ENode.Atom(ConstantType(Constant(c1: T))), ENode.Atom(ConstantType(Constant(c2: T)))) =>
        constant(fn(c1, c2))
      case _ =>
        ENode.OpApply(op, args)

  private def decomposeIntProduct(args: List[ENode]): (Int, List[ENode]) =
    val factors =
      args.flatMap:
        case ENode.OpApply(Op.IntProduct, innerFactors) => innerFactors
        case arg => List(arg)
    val (consts, nonConsts) =
      factors.partitionMap:
        case ENode.Atom(ConstantType(Constant(c: Int))) => Left(c)
        case factor => Right(factor)
    (consts.product, nonConsts.sortBy(idOf.apply))

  private def makeIntProduct(const: Int, nonConsts: List[ENode]): ENode =
    if const == 0 then
      zeroIntNode
    else if const == 1 then
      if nonConsts.isEmpty then oneIntNode
      else if nonConsts.size == 1 then nonConsts.head
      else ENode.OpApply(Op.IntProduct, nonConsts)
    else
      val constNode = constant(const)
      nonConsts match
        case Nil =>
          constNode
        //case List(ENode.OpApply(Op.IntSum, summands)) =>
        //  ENode.OpApply(
        //    Op.IntSum,
        //    summands.map(summand => unique(makeIntProduct(const, List(summand))))
        //  )
        case _ =>
          ENode.OpApply(Op.IntProduct, constNode :: nonConsts)

  private def decomposeIntSum(args: List[ENode]): (Int, List[ENode]) =
    val summands: List[ENode] =
      args.flatMap:
        case ENode.OpApply(Op.IntSum, innerSummands) => innerSummands
        case arg => List(arg)
    val decomposed: List[(Int, List[ENode])] =
      summands.map:
        case ENode.OpApply(Op.IntProduct, args) =>
          args match
            case ENode.Atom(ConstantType(Constant(const: Int))) :: nonConsts => (const, nonConsts)
            case nonConsts => (1, nonConsts)
        case ENode.Atom(ConstantType(Constant(const: Int))) => (const, Nil)
        case other => (1, List(other))
    val grouped = decomposed.groupMapReduce(_._2)(_._1)(_ + _)
    val const = grouped.getOrElse(Nil, 0)
    val nonConsts =
      grouped
        .toList
        .filter((nonConsts, const) => const != 0 && !nonConsts.isEmpty)
        .sortBy((nonConsts, const) => idOf(nonConsts.head))
        .map((nonConsts, const) => unique(makeIntProduct(const, nonConsts)))
    (const, nonConsts)

  private def makeIntSum(const: Int, nonConsts: List[ENode]): ENode =
    if const == 0 then
      if nonConsts.isEmpty then zeroIntNode
      else if nonConsts.size == 1 then nonConsts.head
      else ENode.OpApply(Op.IntSum, nonConsts)
    else
      val constNode = constant(const)
      if nonConsts.isEmpty then constNode
      else ENode.OpApply(Op.IntSum, constNode :: nonConsts)
