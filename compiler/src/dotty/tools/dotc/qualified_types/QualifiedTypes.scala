package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{
  Apply,
  Block,
  EmptyTree,
  Ident,
  If,
  Lambda,
  Literal,
  New,
  Select,
  SeqLiteral,
  This,
  Throw,
  Tree,
  TypeApply,
  Typed,
  given
}
import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.{i, em, toTermName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Types.{AndType, ConstantType, SkolemType, ErrorType, MethodType, OrType, TermRef, Type, TypeProxy}

import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.config.Printers

object QualifiedTypes:
  /** Does the type `tp1` imply the qualifier `qualifier2`?
   *
   *  Used by [[dotty.tools.dotc.core.TypeComparer]] to compare qualified types.
   *
   *  Note: the logic here is similar to [[Type#derivesAnnotWith]] but
   *  additionally handle comparisons with [[SingletonType]]s.
   */
  def typeImplies(tp1: Type, qualifier2: Tree)(using Context): Boolean =
    trace(i"typeImplies $tp1  -->  $qualifier2", Printers.qualifiedTypes):
      tp1 match
        case QualifiedType(parent1, qualifier1) =>
          QualifierSolver().implies(qualifier1, qualifier2)
        case tp1: (ConstantType | TermRef) =>
          QualifierSolver().implies(equalToPredicate(tpd.singleton(tp1)), qualifier2)
          || typeImplies(tp1.underlying, qualifier2)
        case tp1: TypeProxy =>
          typeImplies(tp1.underlying, qualifier2)
        case AndType(tp11, tp12) =>
          typeImplies(tp11, qualifier2) || typeImplies(tp12, qualifier2)
        case OrType(tp11, tp12) =>
          typeImplies(tp11, qualifier2) && typeImplies(tp12, qualifier2)
        case _ =>
          false
          // QualifierSolver().implies(truePredicate(), qualifier2)

  /** Try to adapt the tree to the given type `pt`
   *
   *  Returns [[EmptyTree]] if `pt` does not contain qualifiers or if the tree
   *  cannot be adapted, or the adapted tree otherwise.
   *
   *  Used by [[dotty.tools.dotc.core.Typer]].
   */
  def adapt(tree: Tree, pt: Type)(using Context): Tree =
    trace(i"adapt $tree to $pt", Printers.qualifiedTypes):
      if containsQualifier(pt) && isSimple(tree) then
        val selfifiedTp = QualifiedType(tree.tpe, equalToPredicate(tree))
        if selfifiedTp <:< pt then tree.cast(selfifiedTp) else EmptyTree
      else
        EmptyTree

  def isSimple(tree: Tree)(using Context): Boolean =
    tree match
      case Apply(fn, args)      => isSimple(fn) && args.forall(isSimple)
      case TypeApply(fn, args)  => isSimple(fn)
      case SeqLiteral(elems, _) => elems.forall(isSimple)
      case Typed(expr, _)       => isSimple(expr)
      case Block(Nil, expr)     => isSimple(expr)
      case _ => tpd.isIdempotentExpr(tree)

  def containsQualifier(tp: Type)(using Context): Boolean =
    tp match
      case QualifiedType(_, _) => true
      case tp: TypeProxy       => containsQualifier(tp.underlying)
      case AndType(tp1, tp2)   => containsQualifier(tp1) || containsQualifier(tp2)
      case OrType(tp1, tp2)    => containsQualifier(tp1) || containsQualifier(tp2)
      case _                   => false


  private def equalToPredicate(tree: Tree)(using Context): Tree =
    Lambda(
      MethodType(List("v".toTermName))(_ => List(tree.tpe), _ => defn.BooleanType),
      (args) => Ident(args(0).symbol.termRef).equal(tree)
    )

  private def truePredicate()(using Context): Tree =
    Lambda(
      MethodType(List("v".toTermName))(_ => List(defn.AnyType), _ => defn.BooleanType),
      (args) => Literal(Constant(true))
    )
