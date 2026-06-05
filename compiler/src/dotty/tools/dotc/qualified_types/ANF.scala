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
      case dd: DefDef if isBareBodyWithSkolem(dd.rhs) =>
        // A bare (lazily-evaluated) body: wrap it into a block so its skolem args
        // lift there, under the def's own owner, rather than hoisting to the
        // def's siblings.
        val dd1 = super.transform(dd).asInstanceOf[DefDef]
        cpy.DefDef(dd1)(rhs = anfBlock(Block(Nil, dd1.rhs))(using ctx.withOwner(dd.symbol)))
      case vd: ValDef if vd.symbol.owner.isClass && isBareBodyWithSkolem(vd.rhs) =>
        // A class field / top-level val: wrap its rhs in a block so its skolem
        // args lift into the field initializer rather than hoisting to sibling
        // fields (which would require re-typing class members). The qualifier on
        // the field's own type is dropped by block avoidance — fine, it's a
        // private construction detail.
        val vd1 = super.transform(vd).asInstanceOf[ValDef]
        cpy.ValDef(vd1)(rhs = anfBlock(Block(Nil, vd1.rhs))(using ctx.withOwner(vd.symbol)))
      case _ =>
        super.transform(tree)

  private def isBareBodyWithSkolem(rhs: Tree)(using Context): Boolean =
    !rhs.isEmpty && !rhs.isInstanceOf[Block] && hasSkolemArg(rhs)

  /** Lift every `@QualifierSkolemIndex` argument in the block into a real `val`,
   *  then rewrite `Skolem -> liftedVal.termRef` and drop the markers.
   *
   *  Args in eager position (a statement, a val's rhs, the block's result) are
   *  hoisted into preceding sibling vals; args in conditional/lazy position (an
   *  `if`/`match` branch) are lifted into a fresh block at that branch, so
   *  nothing is hoisted out of a branch and tail calls stay in tail position.
   */
  private def anfBlock(blk: Block)(using Context): Tree =
    if !hasSkolemArg(blk) then return blk
    val (stats, _) = liftStats(blk.stats)
    val (exprDefs, expr1) = hoist(blk.expr, ctx.owner)
    finish(cpy.Block(blk)(stats ++ exprDefs, expr1))

  /** Hoist each statement's skolem args into preceding sibling `val`s. */
  private def liftStats(stats: List[Tree])(using Context): (List[Tree], Boolean) =
    val newStats = ListBuffer[Tree]()
    var lifted = false
    for stat <- stats do
      stat match
        case vd: ValDef if !vd.rhs.isEmpty && hasSkolemArg(vd.rhs) =>
          val (defs, rhs1) = hoist(vd.rhs, vd.symbol)
          newStats ++= defs
          newStats += cpy.ValDef(vd)(rhs = rhs1)
          lifted = true
        case _ if !stat.isInstanceOf[MemberDef] && hasSkolemArg(stat) =>
          val (defs, stat1) = hoist(stat, ctx.owner)
          newStats ++= defs
          newStats += stat1
          lifted = true
        case _ =>
          newStats += stat
    (newStats.toList, lifted)

  /** Rewrite `Skolem -> liftedVal.termRef` over `tree` and drop the now-redundant
   *  `@QualifierSkolemIndex` markers. Each lifted val's symbol carries the marker
   *  transferred by `Lifter.lift`, mapping it to its `TermRef`.
   */
  private def finish(tree: Tree)(using Context): Tree =
    val map = collectSkolemMap(tree)
    tree.foreachSubTree:
      case vd: ValDef => vd.symbol.removeAnnotation(defn.QualifierSkolemIndexAnnot)
      case _ => ()
    val result = TreeTypeMap(typeMap = skolemSubst(map)).transform(tree)
    // `TreeTypeMap` recreates the symbols whose info changed and drops their
    // `defTree`. The qualifier solver reads `defTree` (in `termAssumptions`) to
    // recover a val's rhs assumptions, so restore them.
    result.foreachSubTree:
      case d: MemberDef => d.setDefTree
      case _ => ()
    result

  /** Eager position: returns the `val`s to place *before* `expr`, and the
   *  rewritten `expr`. Recurses through `if`/`match`, lifting their condition or
   *  selector eagerly but each branch into its own local block (`liftBranch`),
   *  and through nested skolem arguments.
   *
   *  `fromOwner` owns `expr`'s nested definitions; a lifted val's body is
   *  reparented from it to the val's own symbol, since `Lifter.lift` only
   *  reparents definitions owned by the enclosing block, not by a sibling.
   */
  private def hoist(expr: Tree, fromOwner: Symbol)(using Context): (List[Tree], Tree) =
    expr match
      case If(cond, thenp, elsep) =>
        val (defs, cond1) = hoist(cond, fromOwner)
        (defs, cpy.If(expr)(cond1, liftBranch(thenp, fromOwner), liftBranch(elsep, fromOwner)))
      case Match(sel, cases) =>
        val (defs, sel1) = hoist(sel, fromOwner)
        val cases1 = cases.mapConserve: cd =>
          cpy.CaseDef(cd)(cd.pat, cd.guard, liftBranch(cd.body, fromOwner))
        (defs, cpy.Match(expr)(sel1, cases1))
      case Typed(e, tpt) =>
        val (defs, e1) = hoist(e, fromOwner)
        (defs, cpy.Typed(expr)(e1, tpt))
      case _: Block =>
        (Nil, expr) // already transformed by `super.transform`
      case Select(qual, name) if isMarkedReceiverSelect(expr) =>
        // A dependent selection on an unstable receiver that is *not* applied (a
        // nullary method or a field). `liftApp` would lift the whole selection,
        // leaving the receiver skolem dangling; lift the marked receiver itself.
        val (defs, qual1) = liftApplication(qual, fromOwner)
        (defs, cpy.Select(expr)(qual1, name))
      case _ =>
        liftApplication(expr, fromOwner)

  /** Lift `expr`'s skolem subexpressions via `LiftSkolem.liftApp`, then recurse
   *  into each lifted val to surface nested skolem args: `liftApp` lifts an
   *  application's args (and a marked receiver) as whole vals, but does not
   *  descend into their own sub-applications.
   */
  private def liftApplication(expr: Tree, fromOwner: Symbol)(using Context): (List[Tree], Tree) =
    val defs = ListBuffer[Tree]()
    val rewritten = LiftSkolem.liftApp(defs, expr)
    val processed = defs.toList.flatMap:
      case vd: ValDef =>
        val inner = peelSkolemTyped(vd.rhs.changeOwner(fromOwner, vd.symbol))
        if hasSkolemArg(inner) && (isAnfable(inner) || isMarkedReceiverSelect(inner)) then
          val (innerDefs, rhs1) = hoist(inner, vd.symbol)
          innerDefs :+ cpy.ValDef(vd)(rhs = rhs1)
        else cpy.ValDef(vd)(rhs = inner) :: Nil
      case d => d :: Nil
    (processed, rewritten)

  /** A `Select` whose receiver carries an `@QualifierSkolemIndex` marker (an
   *  unstable receiver of a dependent member, marked in `typedSelectWithAdapt`).
   */
  private def isMarkedReceiverSelect(tree: Tree)(using Context): Boolean = tree match
    case Select(qual, _) => QualifiedTypes.readSkolemIndexAnnot(qual).isDefined
    case _ => false

  /** Conditional/lazy position: lift `expr`'s skolem args into a fresh block, so
   *  they are not hoisted out of the branch (which would change evaluation order
   *  and move tail calls out of tail position).
   */
  private def liftBranch(expr: Tree, fromOwner: Symbol)(using Context): Tree =
    if !hasSkolemArg(expr) then expr
    else
      val (defs, e) = hoist(expr, fromOwner)
      if defs.isEmpty then e else Block(defs, e)

  /** Shapes `hoist` can descend into. Recursing into anything else (a leaf still
   *  carrying a marker) would re-lift it whole and loop.
   */
  private def isAnfable(tree: Tree): Boolean = tree match
    case _: Apply | _: TypeApply | _: If | _: Match => true
    case _ => false

  private def hasSkolemArg(tree: Tree)(using Context): Boolean =
    tree.existsSubTree(t => QualifiedTypes.readSkolemIndexAnnot(t).isDefined)

  /** Map each lifted skolem `val` reachable from `tree` to its `TermRef`. */
  private def collectSkolemMap(tree: Tree)(using Context): Map[(Symbol, Int), TermRef] =
    val m = scala.collection.mutable.Map[(Symbol, Int), TermRef]()
    tree.foreachSubTree:
      case vd: ValDef =>
        QualifiedTypes.symbolSkolemIndexOpt(vd.symbol).foreach(k => m(k) = vd.symbol.termRef)
      case _ => ()
    m.toMap

  /** A `TypeMap` replacing the `Skolem`s in `map` with their val's `TermRef`, and
   *  dropping `@QualifierSkolemIndex` annotations. Inside a qualifier,
   *  `AnnotatedType` mapping routes the ENode types through this map, so the
   *  qualifier's skolem atoms are rewritten.
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

/** Like `LiftComplex`, but also lifts pure `@QualifierSkolemIndex` arguments:
 *  every skolem-bearing arg must become a `val` (even a pure one like `1: Int`),
 *  so its skolem can be substituted by a stable reference.
 */
private object LiftSkolem extends LiftComplex:
  override def noLift(expr: Tree)(using Context): Boolean =
    super.noLift(expr) && QualifiedTypes.readSkolemIndexAnnot(expr).isEmpty
