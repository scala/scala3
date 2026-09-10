package dotty.tools.dotc
package qualified_types

import dotty.tools.dotc.ast.tpd.{MemberDef, Select, Tree, TreeTraverser, TypeApply, TypeTree}
import dotty.tools.dotc.cc.{captureSet, useSet}
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{defn, toDenot, Symbol}
import dotty.tools.dotc.core.Types.{TermRef, ThisType, Type}
import dotty.tools.dotc.report
import dotty.tools.dotc.util.{EqHashSet, SrcPos}

/** Checks that predicates of qualified types are pure, using capture-checking
 *  use-sets: every method applied in a predicate must have an empty use-set,
 *  every referenced value must have an empty capture set, and mutable
 *  variables must not be read.
 *
 *  Only qualifiers in declared or inferred types are checked. Qualifiers in
 *  synthesized cast types (selfification during adaptation) are skipped: an
 *  impure predicate they could prove must mention the impure call as well,
 *  and thus appears in a checked type too.
 *
 *  Must be invoked from `CheckCaptures.postCheck`: it reads `useSet`s, which
 *  are only available while the capture-checking phase runs, and are final
 *  only after all rechecking iterations of the unit have completed.
 *
 *  Known limitations, inherited from the capture-checking model:
 *  - Exceptions are not tracked: an empty use-set does not imply totality.
 *  - `this`-mediated mutation (e.g. `def next() = { c += 1; c }`) is only
 *    tracked under mutation tracking (`Mutable` classes), not under plain
 *    `captureChecking`.
 *  - Separate compilation trusts declared `uses` clauses: a method without
 *    one is assumed pure.
 */
object PurityCheck:
  def check(unit: Tree)(using Context): Unit =
    val checkedQualifiers = EqHashSet[ENode]()
    val checker = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit = tree match
        case TypeApply(fun @ Select(qual, _), _) if fun.symbol == defn.Any_typeCast =>
          traverse(qual)
        case _ =>
          traverseChildren(tree)
          tree match
            case tree: TypeTree =>
              checkType(tree.tpe, tree.srcPos, checkedQualifiers)
            case tree: MemberDef if tree.symbol.exists =>
              checkType(tree.symbol.info, tree.srcPos, checkedQualifiers)
            case _ => ()
    checker.traverse(unit)

  private def checkType(tp: Type, pos: SrcPos, checked: EqHashSet[ENode])(using Context): Unit =
    tp.foreachPart:
      case QualifiedType(_, qualifier) =>
        if checked.add(qualifier) then checkQualifier(qualifier, pos)
      case _ => ()

  /** Checks the two purity conditions over the qualifier's body, mirroring
   *  what `markFree` would charge to the environment of an equivalent
   *  closure: applied methods must have an empty use-set, and referenced
   *  values must have an empty capture set.
   */
  private def checkQualifier(qualifier: ENode.Lambda, pos: SrcPos)(using Context): Unit =
    def checkCallee(sym: Symbol): Unit =
      if sym.exists && !sym.useSet.elems.isEmpty then
        report.error(
          em"Qualified type predicate is not pure: call to ${sym.showLocated}, which uses capabilities ${sym.useSet}",
          pos
        )

    def checkRef(tp: Type): Unit = tp match
      case tp: (TermRef | ThisType) =>
        if !tp.captureSet.elems.isEmpty then
          report.error(
            em"Qualified type predicate is not pure: reference to capability $tp",
            pos
          )
      case _ => ()

    def walkFn(fn: ENode): Unit = fn match
      case ENode.Atom(tp: TermRef) => checkCallee(tp.symbol)
      case ENode.Select(qual, member) => checkCallee(member); walk(qual)
      case ENode.TypeApply(fn1, _) => walkFn(fn1)
      case _ => walk(fn)

    def walk(n: ENode): Unit = n match
      case ENode.Atom(tp) => checkRef(tp)
      case n: ENode.Constructor => checkCallee(n.constr)
      case ENode.Select(qual, member) =>
        if member.is(Flags.Mutable) then
          report.error(
            em"Qualified type predicate is not pure: read of mutable ${member.showLocated}",
            pos
          )
        else if member.is(Flags.Method) then checkCallee(member)
        walk(qual)
      case ENode.Apply(fn, args) => walkFn(fn); args.foreach(walk)
      case ENode.OpApply(_, args) => args.foreach(walk)
      case ENode.TypeApply(fn, _) => walkFn(fn)
      case ENode.Lambda(_, _, body) => walk(body)

    walk(qualifier.body)
