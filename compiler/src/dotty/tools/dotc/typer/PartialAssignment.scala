package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.ApplyKind
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import core.Symbols.defn

/** A function computing the assignment of a lvalue.
  *
  * @param lhs The target of the assignment.
  * @param perform: A closure that accepts `lhs` and an untyped tree `rhs`, and returns a tree
  *   representing the assignment of `rhs` to `lhs`.
  */
private[typer] final class PartialAssignment[+T <: LValue](val lhs: T)(
    perform: (T, untpd.Tree) => untpd.Tree
):

  /** Returns a tree computing the assignment of `rhs` to `lhs`. */
  def apply(rhs: untpd.Tree): untpd.Tree =
    perform(lhs, rhs)

end PartialAssignment

/** The left-hand side of an assignment. */
private[typer] sealed abstract class LValue:

  /** Returns the local `val` definitions composing this lvalue. */
  def locals: List[tpd.ValDef]

  /** Returns a tree computing the assignment of `rhs` to this lvalue. */
  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree

  /** Returns the value of `t`, which may be an expression or a local `val` definition. */
  protected final def read(t: tpd.Tree)(using Context): tpd.Tree =
    t match
      case d: tpd.ValDef => tpd.Ident(d.namedType)
      case e => e

end LValue

private[typer] final case class UnappliedSetter(
    expression: untpd.Tree, locals: List[tpd.ValDef]
) extends LValue:

  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree =
    untpd.Apply(expression, List(rhs))

end UnappliedSetter

/** A simple expression, typically valid on left-hand side of an `Assign` tree.
  *
  * Use this class to represent an assignment that translates to an `Assign` tree or to wrap an
  * error whose diagnostic can be delayed until the right-hand side is known.
  *
  * @param expression The expression of the lvalue.
  */
private[typer] final case class SimpleLValue(expression: tpd.Tree) extends LValue:

  def locals: List[tpd.ValDef] =
    List()

  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree =
    val s = untpd.Assign(untpd.TypedSplice(expression), rhs)
    untpd.TypedSplice(s.withType(defn.UnitType))

end SimpleLValue

/** A lvalue represeted by the partial application a function.
  *
  * @param function The partially applied function.
  * @param arguments The arguments of the partial application.
  * @param kind The way in which the function is applied.
  */
private[typer] final case class ApplyLValue(
    function: tpd.Tree,
    arguments: List[tpd.Tree],
    kind: ApplyKind = ApplyKind.Regular
) extends LValue:

  val locals: List[tpd.ValDef] =
    (function +: arguments).collect { case d: tpd.ValDef => d }

  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree =
    val s = untpd.TypedSplice(read(function))
    val t = arguments.map((a) => untpd.TypedSplice(read(a))) :+ rhs
    untpd.Apply(s, t)

end ApplyLValue

/** A lvalue represeted by the application of a partially applied method.
  *
  * @param receiver The receiver of the partially applied method.
  * @param member The name of the partially applied method.
  * @param arguments The arguments of the partial application.
  */
private[typer] final case class SelectLValue(
    receiver: tpd.Tree,
    member: Name,
    arguments: List[tpd.Tree] = List()
) extends LValue:

  def expandReceiver()(using Context): tpd.Tree =
    receiver match
      case d: tpd.ValDef => d.rhs
      case r => r

  val locals: List[tpd.ValDef] =
    (receiver +: arguments).collect { case d: tpd.ValDef => d }

  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree =
    val s = untpd.Select(untpd.TypedSplice(read(receiver)), member)
    val t = arguments.map((a) => untpd.TypedSplice(read(a))) :+ rhs
    untpd.Apply(s, t)

end SelectLValue
