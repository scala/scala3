package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{Tree, Typed, TypeTree, ValDef, given}
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Types.{SingletonType, Type}
import dotty.tools.dotc.util.Property

/** Recorded facts, to be used by the [[QualifierSolver]]. */
private enum Fact:

  /** Value fact: the value defined by `tree` is in scope. */
  case Value(tree: ValDef)

  /** Condition fact: the condition `tree` is known to be `pos`. */
  case Condition(tree: Tree, pos: Boolean)

  /** Case fact: the `pattern` matched the `scrutinee` if and only if `pos`. */
  case Case(scrutinee: Tree, pattern: Tree, pos: Boolean)

  /** The [[ENode]] representation of this fact, if it can be converted. */
  def toENode(using Context): Option[ENode] =
    this match
      case Value(vdef) =>
        None // TODO
      case Condition(cond, pos) =>
        val base = ENode.fromTree(cond)
        if pos then base
        else base.map(n => ENode.OpApply(ENode.Op.Not, List(n)))
      case Case(scrutinee, pattern, pos) =>
        None // TODO

type QualifierContext = List[Fact]

object QualifierContext:

  /** A key to be used in a context property that tracks the current qualifier context */
  private val key = new Property.Key[QualifierContext]

  /** Returns the current qualifier context facts. */
  def facts(using Context): List[Fact] =
    ctx.property(key).getOrElse(Nil)

  inline def afterMemberContext(member: Tree)(using Context): Context =
    member match
      case vdef: ValDef => withFact(Fact.Value(vdef))
      case _ => ctx

  inline def caseContext(scrutinee: Tree, pattern: Tree)(using Context): Context =
    withFact(Fact.Case(scrutinee, pattern, pos = true))

  inline def afterCaseContext(scrutinee: Tree, pattern: Tree)(using Context): Context =
    withFact(Fact.Case(scrutinee, pattern, pos = false))

  inline def trueContext(fact: Tree)(using Context): Context =
    withFact(Fact.Condition(fact, pos = true))

  inline def falseContext(fact: Tree)(using Context): Context =
    withFact(Fact.Condition(fact, pos = false))

  /** Cast `tree` to `branchPt` if it is a qualified type, so that branch
   *  types carry the qualifier through join. Analogous to `gadtAdaptBranch`.
   */
  inline def adaptBranch(tree: Tree, branchPt: Type)(using Context): Tree =
    if Feature.qualifiedTypesEnabled
      && QualifiedTypes.containsQualifier(branchPt)
      && !(tree.tpe <:< branchPt)
    then
      Typed(tree, TypeTree(branchPt))
    else
      tree

  private inline def withFact(fact: => Fact)(using Context): Context =
    if Feature.qualifiedTypesEnabled then
      val prev = ctx.property(key).getOrElse(Nil)
      ctx.fresh.setProperty(key, fact :: prev)
    else ctx
