package dotty.tools.dotc.qualified_types

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{
  CachedProxyType,
  ConstantType,
  MethodType,
  NamedType,
  NoPrefix,
  SingletonType,
  SkolemType,
  TermParamRef,
  TermRef,
  Type,
  TypeVar,
  ValueType
}
import dotty.tools.dotc.qualified_types.ENode.Op
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.transform.TreeExtractors.BinaryOp
import dotty.tools.dotc.util.Spans.Span

final class EGraph(rootCtx: Context):

  private val represententOf = mutable.Map.empty[ENode, ENode]

  private def representent(node: ENode): ENode =
    represententOf.get(node) match
      case None => node
      case Some(repr) =>
        assert(repr ne node, s"Node $node has itself as representent")
        representent(repr)

  /** Map from child nodes to their parent nodes */
  private val usedBy = mutable.Map.empty[ENode, mutable.Set[ENode]]

  private def uses(node: ENode): mutable.Set[ENode] =
    usedBy.getOrElseUpdate(node, mutable.Set.empty)

  private def addUse(node: ENode, parent: ENode): Unit =
    require(!represententOf.contains(node), s"Reference $node is not normalized")
    uses(node) += parent

  /** Map used for hash-consing nodes, keys and values are the same */
  private val index = mutable.Map.empty[ENode, ENode]

  val trueNode: ENode.Atom = ENode.Atom(ConstantType(Constant(true))(using rootCtx))
  index(trueNode) = trueNode

  val falseNode: ENode.Atom = ENode.Atom(ConstantType(Constant(false))(using rootCtx))
  index(falseNode) = falseNode

  val minusOneIntNode: ENode.Atom = ENode.Atom(ConstantType(Constant(-1))(using rootCtx))
  index(minusOneIntNode) = minusOneIntNode

  val zeroIntNode: ENode.Atom = ENode.Atom(ConstantType(Constant(0))(using rootCtx))
  index(zeroIntNode) = zeroIntNode

  val oneIntNode: ENode.Atom = ENode.Atom(ConstantType(Constant(1))(using rootCtx))
  index(oneIntNode) = oneIntNode

  val d = defn(using rootCtx) // Need a stable path to match on `defn` members
  val builtinOps = Map(
    d.Int_== -> Op.Equal,
    d.Boolean_== -> Op.Equal,
    d.Any_== -> Op.Equal,
    d.Boolean_&& -> Op.And,
    d.Boolean_|| -> Op.Or,
    d.Boolean_! -> Op.Not,
    d.Int_+ -> Op.IntSum,
    d.Int_* -> Op.IntProduct
  )

  private val worklist = mutable.Queue.empty[ENode]

  override def toString(): String =
    val represententsString = represententOf.map((node, repr) => s" $node -> $repr").mkString("\n")
    s"EGraph:\n$represententsString\n"

  def equiv(node1: ENode, node2: ENode)(using Context): Boolean =
    trace(i"EGraph.equiv", Printers.qualifiedTypes):
      val margin = ctx.base.indentTab * (ctx.base.indent)
      // println(s"$margin node1: $node1\n$margin node2: $node2")
      // Check if the representents of both nodes are the same
      val repr1 = representent(node1)
      val repr2 = representent(node2)
      repr1 eq repr2

  private def unique(node: ENode): node.type =
    index.getOrElseUpdate(
      node, {
        node match
          case ENode.Atom(tp) =>
            ()
          case ENode.New(clazz) =>
            addUse(clazz, node)
          case ENode.Select(qual, member) =>
            addUse(qual, node)
          case ENode.Apply(fn, args) =>
            addUse(fn, node)
            args.foreach(addUse(_, node))
          case ENode.OpApply(op, args) =>
            args.foreach(addUse(_, node))
          case ENode.TypeApply(fn, args) =>
            addUse(fn, node)
          case ENode.Lambda(paramTps, retTp, body) =>
            addUse(body, node)
        node
      }
    ).asInstanceOf[node.type]

  def toNode(tree: Tree, paramSyms: List[Symbol] = Nil, paramNodes: List[ENode.ArgRefType] = Nil)(using
      Context
  ): Option[ENode] =
    trace(i"EGraph.toNode $tree", Printers.qualifiedTypes):
      computeToNode(tree, paramSyms, paramNodes).map(node => representent(unique(node)))

  private def computeToNode(
      tree: Tree,
      paramSyms: List[Symbol] = Nil,
      paramNodes: List[ENode.ArgRefType] = Nil
  )(using currentCtx: Context): Option[ENode] =
    trace(i"ENode.computeToNode $tree", Printers.qualifiedTypes):
      def normalizeType(tp: Type): Type =
        tp match
          case tp: TypeVar if tp.isPermanentlyInstantiated =>
            tp.permanentInst
          case tp: NamedType =>
            if tp.symbol.isStatic then tp.symbol.termRef
            else normalizeType(tp.prefix).select(tp.symbol)
          case tp => tp

      def mapType(tp: Type): Type =
        normalizeType(tp.subst(paramSyms, paramNodes))

      tree match
        case Literal(_) | Ident(_) | This(_) if tree.tpe.isInstanceOf[SingletonType] =>
          Some(ENode.Atom(mapType(tree.tpe).asInstanceOf[SingletonType]))
        case New(clazz) =>
          for clazzNode <- toNode(clazz, paramSyms, paramNodes) yield ENode.New(clazzNode)
        case Select(qual, name) =>
          for qualNode <- toNode(qual, paramSyms, paramNodes) yield ENode.Select(qualNode, tree.symbol)
        case BinaryOp(lhs, op, rhs) if builtinOps.contains(op) =>
          for
            lhsNode <- toNode(lhs, paramSyms, paramNodes)
            rhsNode <- toNode(rhs, paramSyms, paramNodes)
          yield normalizeOp(builtinOps(op), List(lhsNode, rhsNode))
        case BinaryOp(lhs, d.Int_-, rhs) if lhs.tpe.isInstanceOf[ValueType] && rhs.tpe.isInstanceOf[ValueType] =>
          for
            lhsNode <- toNode(lhs, paramSyms, paramNodes)
            rhsNode <- toNode(rhs, paramSyms, paramNodes)
          yield normalizeOp(Op.IntSum, List(lhsNode, normalizeOp(Op.IntProduct, List(minusOneIntNode, rhsNode))))
        case Apply(fun, args) =>
          for
            funNode <- toNode(fun, paramSyms, paramNodes)
            argsNodes <- args.map(toNode(_, paramSyms, paramNodes)).sequence
          yield ENode.Apply(funNode, argsNodes)
        case TypeApply(fun, args) =>
          for funNode <- toNode(fun, paramSyms, paramNodes)
          yield ENode.TypeApply(funNode, args.map(tp => mapType(tp.tpe)))
        case closureDef(defDef) =>
          defDef.symbol.info.dealias match
            case mt: MethodType =>
              assert(defDef.termParamss.size == 1, "closures have a single parameter list, right?")
              val params = defDef.termParamss.head
              val myParamSyms = params.map(_.symbol)

              val myParamTps: ArrayBuffer[Type] = ArrayBuffer.empty
              ???

              val myRetTp = ???

              val myParamNodes = myParamTps.zipWithIndex.map((tp, i) => ENode.ArgRefType(i, tp)).toList

              for body <- toNode(defDef.rhs, myParamSyms ::: paramSyms, myParamNodes ::: paramNodes)
              yield ENode.Lambda(myParamTps.toList, myRetTp, body)
            case _ => None
        case _ =>
          None

  private def canonicalize(node: ENode): ENode =
    representent(unique(
      node match
        case ENode.Atom(tp) =>
          node
        case ENode.New(clazz) =>
          ENode.New(representent(clazz))
        case ENode.Select(qual, member) =>
          ENode.Select(representent(qual), member)
        case ENode.Apply(fn, args) =>
          ENode.Apply(representent(fn), args.map(representent))
        case ENode.OpApply(op, args) =>
          normalizeOp(op, args.map(representent))
        case ENode.TypeApply(fn, args) =>
          ENode.TypeApply(representent(fn), args)
        case ENode.Lambda(paramTps, retTp, body) =>

          ENode.Lambda(paramTps, retTp, representent(body))
    ))

  private def normalizeOp(op: ENode.Op, args: List[ENode]): ENode =
    op match
      case Op.Equal =>
        assert(args.size == 2, s"Expected 2 arguments for equality, got $args")
        if args(0) eq args(1) then trueNode
        else ENode.OpApply(op, args.sortBy(_.hashCode()))
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
      case Op.IntProduct =>
        val (consts, nonConsts) = decomposeIntProduct(args)
        makeIntProduct(consts, nonConsts)
      case Op.IntSum =>
        val (const, nonConsts) = decomposeIntSum(args)
        makeIntSum(const, nonConsts)
      case _ =>
        ENode.OpApply(op, args)

  private def decomposeIntProduct(args: List[ENode]): (Int, List[ENode]) =
    val factors =
      args.flatMap:
        case ENode.OpApply(Op.IntProduct, innerFactors) => innerFactors
        case arg                                        => List(arg)
    val (consts, nonConsts) =
      factors.partitionMap:
        case ENode.Atom(ConstantType(Constant(c: Int))) => Left(c)
        case factor                                     => Right(factor)
    (consts.product, nonConsts.sortBy(_.hashCode()))

  private def makeIntProduct(const: Int, nonConsts: List[ENode]): ENode =
    if const == 0 then
      zeroIntNode
    else if const == 1 then
      if nonConsts.isEmpty then oneIntNode
      else if nonConsts.size == 1 then nonConsts.head
      else ENode.OpApply(Op.IntProduct, nonConsts)
    else
      val constNode = unique(ENode.Atom(ConstantType(Constant(const))(using rootCtx)))
      nonConsts match
        case Nil =>
          constNode
        case List(ENode.OpApply(Op.IntSum, summands)) =>
          ENode.OpApply(Op.IntSum, summands.map(summand => normalizeOp(Op.IntProduct, List(constNode, summand))))
        case _ =>
          ENode.OpApply(Op.IntProduct, constNode :: nonConsts)

  private def decomposeIntSum(args: List[ENode]): (Int, List[ENode]) =
    val summands: List[ENode] =
      args.flatMap:
        case ENode.OpApply(Op.IntSum, innerSummands) => innerSummands
        case arg                                     => List(arg)
    val decomposed: List[(Int, List[ENode])] =
      summands.map:
        case ENode.OpApply(Op.IntProduct, args) =>
          args match
            case ENode.Atom(ConstantType(Constant(const: Int))) :: nonConsts => (const, nonConsts)
            case nonConsts                                                   => (1, nonConsts)
        case ENode.Atom(ConstantType(Constant(const: Int))) => (const, Nil)
        case other                                          => (1, List(other))
    val grouped = decomposed.groupMapReduce(_._2)(_._1)(_ + _)
    val const = grouped.getOrElse(Nil, 0)
    val nonConsts =
      grouped
        .toList
        .filter((nonConsts, const) => const != 0 && !nonConsts.isEmpty)
        .sortBy((nonConsts, const) => nonConsts.hashCode())
        .map((nonConsts, const) => makeIntProduct(const, nonConsts))
    (const, nonConsts)

  private def makeIntSum(const: Int, nonConsts: List[ENode]): ENode =
    if const == 0 then
      if nonConsts.isEmpty then zeroIntNode
      else if nonConsts.size == 1 then nonConsts.head
      else ENode.OpApply(Op.IntSum, nonConsts)
    else
      val constNode = unique(ENode.Atom(ConstantType(Constant(const))(using rootCtx)))
      if nonConsts.isEmpty then constNode
      else ENode.OpApply(Op.IntSum, constNode :: nonConsts)

  private def order(a: ENode, b: ENode): (ENode, ENode) =
    (a, b) match
      case (ENode.Atom(_: ConstantType), _) => (a, b)
      case (_, ENode.Atom(_: ConstantType)) => (b, a)
      case (ENode.Atom(_: SkolemType), _)   => (a, b)
      case (_, ENode.Atom(_: SkolemType))   => (b, a)
      case (_: ENode.Atom, _)               => (a, b)
      case (_, _: ENode.Atom)               => (b, a)
      case (_: ENode.New, _)                => (a, b)
      case (_, _: ENode.New)                => (b, a)
      case (_: ENode.Select, _)             => (a, b)
      case (_, _: ENode.Select)             => (b, a)
      case (_: ENode.Apply, _)              => (a, b)
      case (_, _: ENode.Apply)              => (b, a)
      case (_: ENode.TypeApply, _)          => (a, b)
      case (_, _: ENode.TypeApply)          => (b, a)
      case _                                => (a, b)

  def merge(a: ENode, b: ENode): Unit =
    val aRepr = representent(a)
    val bRepr = representent(b)
    if aRepr eq bRepr then return
    assert(aRepr != bRepr, s"$aRepr and $bRepr are `equals` but not `eq`")

    // TODO(mbovel): if both nodes are objects, recursively merge their arguments

    /// Update represententOf and usedBy maps
    val (newRepr, oldRepr) = order(aRepr, bRepr)
    represententOf(oldRepr) = newRepr
    uses(newRepr) ++= uses(oldRepr)
    val oldUses = uses(oldRepr)
    usedBy.remove(oldRepr)

    // Propagate truth values over disjunctions, conjunctions and equalities
    oldRepr match
      case ENode.OpApply(Op.And, args) if newRepr eq trueNode =>
        args.foreach(merge(_, trueNode))
      case ENode.OpApply(Op.Or, args) if newRepr eq falseNode =>
        args.foreach(merge(_, falseNode))
      case ENode.OpApply(Op.Equal, args) if newRepr eq trueNode =>
        args.foreach(arg => merge(args(0), args(1)))
      case _ =>
        ()

    // Enqueue all nodes that use the oldRepr for repair
    worklist.enqueueAll(oldUses)

  def repair(): Unit =
    while !worklist.isEmpty do
      val head = worklist.dequeue()
      val headRepr = representent(head)
      val headCanonical = canonicalize(head)
      if headRepr ne headCanonical then
        merge(headRepr, headCanonical)

  private def toTree(node: ENode, paramRefs: List[Tree])(using Context): Tree =
    node match
      case ENode.Atom(tp) =>
        singleton(tp)
      case ENode.New(clazz) =>
        New(toTree(clazz, paramRefs))
      case ENode.Select(qual, member) =>
        toTree(qual, paramRefs).select(member)
      case ENode.Apply(fn, args) =>
        Apply(toTree(fn, paramRefs), args.map(toTree(_, paramRefs)))
      case ENode.OpApply(op, args) =>
        ???
      case ENode.TypeApply(fn, args) =>
        TypeApply(toTree(fn, paramRefs), args.map(TypeTree(_, false)))
      case ENode.Lambda(paramTps, retTp, body) =>
        ???

  extension [T](xs: List[Option[T]])
    private def sequence: Option[List[T]] =
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
