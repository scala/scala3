package dotty.tools.dotc
package qualified_types

import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Types.{AnnotatedType, TermRef, Type, TypeMap}
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.typer.LiftComplex

/** Converts the skolem encoding of dependent-function arguments into real
 *  A-normal-form `val` bindings, for tools that consume post-inlining trees
 *  (e.g. Stainless) and can't handle `ENodeVar.Skolem`.
 *
 *  A skolem stands for the unstable argument of a dependent function call.
 *  Given `def id(x: Int): {r: Int with r == x}`, this:
 *
 *  ```scala
 *  val c = id(impure())
 *  ```
 *
 *  is elaborated with a skolem standing in for the unstable argument `impure()`,
 *  tied to the argument tree by a `@QualifierSkolemIndex` annotation:
 *
 *  ```scala
 *  val c: {r: Int with r == skolem(1)} = id(impure(): Int @QualifierSkolemIndex(1))
 *  ```
 *
 *  This phase lifts that argument into a stable `val` and rewrites the qualifier
 *  to reference it instead of the skolem:
 *
 *  ```scala
 *  val skolem$1: Int = impure()
 *  val c: {r: Int with r == skolem$1} = id(skolem$1)
 *  ```
 *
 *  The result is exactly what a stable argument would have produced, so the
 *  consumer never sees the skolem encoding. Only runs under
 *  `-Yqualified-types-anf`; standard TASTy (pickled before inlining) is
 *  unaffected.
 *
 *  Skolems with no producing argument (`SkolemType`/non-singleton demotions
 *  in `ENode.singleton`) carry no `@QualifierSkolemIndex` and are left alone.
 */
class ANF extends MacroTransform, IdentityDenotTransformer:
  thisPhase =>

  override def phaseName: String = ANF.name
  override def description: String = ANF.description
  override def changesMembers: Boolean = true

  override def isRunnable(using Context): Boolean =
    super.isRunnable && ctx.settings.YqualifiedTypesAnf.value

  def newTransformer(using Context): Transformer = new Transformer:
    override def transform(tree: Tree)(using Context): Tree = tree match
      case tree @ Annotated(arg, _) =>
        // Don't descend into annotation trees: a qualifier `@qualified[T](pred)`
        // carries the predicate as a closure, and its skolems are logical, not
        // runtime values to lift.
        cpy.Annotated(tree)(transform(arg), tree.annot)
      case blk: Block =>
        anfBlock(super.transform(blk).asInstanceOf[Block])
      case dd: DefDef if !dd.rhs.isEmpty && !dd.rhs.isInstanceOf[Block] =>
        // A lazily-evaluated bare body: wrap into a block so any skolem args can
        // be lifted there (the body is evaluated on call, so the vals must stay
        // inside it, not hoist to the def's siblings). Lift under the def's own
        // owner so the lifted vals belong to its body.
        val dd1 = super.transform(dd).asInstanceOf[DefDef]
        cpy.DefDef(dd1)(rhs = anfBlock(Block(Nil, dd1.rhs))(using ctx.withOwner(dd.symbol)))
      case _ =>
        super.transform(tree)

  /** Hoist each statement's skolem-producing argument(s) into preceding sibling
   *  `val`s, then rewrite `Skolem -> liftedVal.termRef` over the whole block
   *  and drop the now-redundant `@QualifierSkolemIndex` markers.
   *
   *  Only args whose skolem actually occurs in a qualifier type (`referenced`)
   *  are lifted: an unstable arg to a dependent function whose result qualifier
   *  doesn't mention it (e.g. `p(v)` over a stable param) needs no rewriting,
   *  and lifting it would needlessly restructure the code (e.g. break tailrec).
   */
  private def anfBlock(blk: Block)(using Context): Tree =
    // When the enclosing owner declares a qualified result type, the block's
    // result must *conform* to it, and its skolems are load-bearing for that
    // check (e.g. a self-referential `fib` whose result qualifier mentions
    // `fib(n - 1)`): the declared type refers to those terms via bound params,
    // so lifting them to body-local vals would break conformance. Such skolems
    // live in a logical predicate, not at a runtime point — leave them encoded.
    if ctx.owner.isTerm && QualifiedTypes.containsQualifier(ctx.owner.info) then return blk
    val referenced = referencedSkolemIds(blk)
    if referenced.isEmpty then return blk
    def needsLift(tree: Tree): Boolean =
      val owner = QualifiedTypes.skolemOwner
      tree.existsSubTree: t =>
        QualifiedTypes.readSkolemIndexAnnot(t).exists(idx => referenced.contains((owner, idx)))
    val newStats = ListBuffer[Tree]()
    var lifted = false
    // `fromOwner` is the symbol that currently owns the statement's nested
    // definitions: the val/def symbol for a member, else the block owner. The
    // lifted vals move under new sibling symbols, so their bodies must be
    // reparented away from `fromOwner` (see `liftSkolemArgs`).
    def liftInto(tree: Tree, fromOwner: Symbol): Tree =
      val (defs, tree1) = liftSkolemArgs(tree, fromOwner, needsLift)
      newStats ++= defs
      lifted = true
      tree1
    for stat <- blk.stats do
      stat match
        case vd: ValDef if !vd.rhs.isEmpty && needsLift(vd.rhs) =>
          newStats += cpy.ValDef(vd)(rhs = liftInto(vd.rhs, vd.symbol))
        case _ if !stat.isInstanceOf[MemberDef] && needsLift(stat) =>
          newStats += liftInto(stat, ctx.owner)
        case _ =>
          newStats += stat
    val expr1 = if needsLift(blk.expr) then liftInto(blk.expr, ctx.owner) else blk.expr
    if !lifted then blk
    else
      val stats = newStats.toList
      val map = collectSkolemMap(stats)
      // Bail if some referenced skolem has no lifted val to map onto (e.g. its
      // producing arg was a pure path that `lift` left inline): substituting
      // only part of the qualifier would leave a dangling skolem. Leave the
      // whole block on its original encoding instead.
      if !referenced.subsetOf(map.keySet) then return blk
      // The index markers have served their purpose; drop them so the lifted
      // vals look like ordinary bindings. (Read the map before stripping.)
      stats.foreach:
        case vd: ValDef => vd.symbol.removeAnnotation(defn.QualifierSkolemIndexAnnot)
        case _ => ()
      val rebuilt = cpy.Block(blk)(stats, expr1)
      val result = TreeTypeMap(typeMap = skolemSubst(map)).transform(rebuilt)
      // `TreeTypeMap` recreates the symbols whose info changed and drops their
      // `defTree`. The qualifier solver reads `defTree` (in `termAssumptions`)
      // to recover a val's rhs assumptions.
      result.foreachSubTree:
        case d: MemberDef => d.setDefTree
        case _ => ()
      result

  /** Lift the skolem-producing arguments of `expr` (recursively, preserving
   *  evaluation order) into `val` bindings, returning the bindings and the
   *  rewritten expression. Uses `LiftComplex` so every effectful subexpression
   *  evaluated before a lifted arg is lifted too.
   *
   *  `fromOwner` owns `expr`'s nested definitions; each lifted val's body is
   *  reparented from it to the val's own symbol, since `lift` only reparents
   *  definitions owned by the enclosing block, not by a sibling val/def.
   */
  private def liftSkolemArgs(expr: Tree, fromOwner: Symbol, needsLift: Tree => Boolean)(using
      Context): (List[Tree], Tree) =
    val defs = ListBuffer[Tree]()
    val rewritten = LiftComplex.liftApp(defs, expr)
    // `liftApp` lifts an application's args as whole vals (their rhs typically
    // wrapped in the `@QualifierSkolemIndex` `Typed`), but does not descend into
    // a lifted arg's own sub-applications. So peel the wrapper and recurse into
    // any lifted val whose value still holds a deeper skolem arg, to surface it
    // as its own val.
    val processed = defs.toList.flatMap:
      case vd: ValDef =>
        val reparented = vd.rhs.changeOwner(fromOwner, vd.symbol)
        val inner = peelSkolemTyped(reparented)
        if needsLift(inner) && isDecomposable(inner) then
          val (innerDefs, rhs1) = liftSkolemArgs(inner, vd.symbol, needsLift)
          innerDefs :+ cpy.ValDef(vd)(rhs = rhs1)
        else
          // Drop the `@QualifierSkolemIndex` `Typed` ascription on this lifted
          // val so it reads as an ordinary binding; its underlying type is the
          // plain arg type, so no conformance ascription is lost.
          cpy.ValDef(vd)(rhs = inner) :: Nil
      case d => d :: Nil
    (processed, rewritten)

  /** Shapes whose arguments `liftApp` can extract; recursing into anything else
   *  would re-lift it whole and loop. (`if`/`match` branches are not handled.)
   */
  private def isDecomposable(tree: Tree): Boolean = tree match
    case _: Apply | _: TypeApply => true
    case _ => false

  /** The skolem identities `(owner, index)` occurring in a qualifier within
   *  `tp`, added to `ids`.
   */
  private def scanSkolems(tp: Type, ids: scala.collection.mutable.Set[(Symbol, Int)])(using Context): Unit =
    tp.foreachPart:
      case QualifiedType(_, qualifier) =>
        qualifier.foreachType:
          case sk: ENodeVar.Skolem => ids += ((sk.owner, sk.index))
          case _ => ()
      case _ => ()

  /** The skolem identities `(owner, index)` that occur in a qualifier type
   *  anywhere in `tree` (in tree types or member-symbol infos) — the skolems
   *  this phase must eliminate.
   */
  private def referencedSkolemIds(tree: Tree)(using Context): Set[(Symbol, Int)] =
    val ids = scala.collection.mutable.Set[(Symbol, Int)]()
    tree.foreachSubTree: t =>
      scanSkolems(t.tpe, ids)
      t match
        case d: MemberDef if d.symbol.exists => scanSkolems(d.symbol.info, ids)
        case _ => ()
    ids.toSet

  /** Map each lifted skolem `val` (its symbol carries the `@QualifierSkolemIndex`
   *  transferred by `Lifter.lift`) to its `TermRef`.
   */
  private def collectSkolemMap(stats: List[Tree])(using Context): Map[(Symbol, Int), TermRef] =
    stats.flatMap:
      case vd: ValDef =>
        QualifiedTypes.symbolSkolemIndexOpt(vd.symbol).map(_ -> vd.symbol.termRef)
      case _ => None
    .toMap

  /** A `TypeMap` replacing the specific `Skolem`s in `map` with their val's
   *  `TermRef`, and dropping `@QualifierSkolemIndex` annotations. Inside a
   *  qualifier, `AnnotatedType` mapping routes the ENode types through this
   *  map, so the qualifier's skolem atoms are rewritten.
   */
  private def skolemSubst(map: Map[(Symbol, Int), TermRef])(using Context): TypeMap =
    new TypeMap:
      def apply(tp: Type): Type = tp match
        case sk: ENodeVar.Skolem if map.contains((sk.owner, sk.index)) =>
          map((sk.owner, sk.index))
        case AnnotatedType(parent, annot) if annot.symbol == defn.QualifierSkolemIndexAnnot =>
          apply(parent)
        case _ => mapOver(tp)

  /** Drop the `Typed(arg, _ @QualifierSkolemIndex(n))` ascription wrapping a
   *  lifted arg, leaving the bare expression.
   */
  private def peelSkolemTyped(tree: Tree)(using Context): Tree = tree match
    case Typed(expr, tpt) if QualifiedTypes.readSkolemIndexAnnot(tpt).isDefined =>
      expr
    case _ => tree

object ANF:
  val name: String = "qualifiedTypesANF"
  val description: String = "ANF-lift qualifier argument skolems into real val bindings"
