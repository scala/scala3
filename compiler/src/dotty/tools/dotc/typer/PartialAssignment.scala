package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.ApplyKind
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.TreeInfo
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.NameKinds.TempResultName

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

/** The expression of a pure value or a synthetic val definition binding a value whose evaluation
 *  must be hoisted.
 *
 *  Use this type to represent a part of a lvalue that must be evaluated before the lvalue gets
 *  used for updating a value.
 */
private[typer] final class PossiblyHoistedValue private (representation: tpd.Tree):

  /** Returns a tree representing the value of `self`. */
  def value(using Context): tpd.Tree =
    definition match
      case Some(d) => tpd.Ident(d.namedType).withSpan(representation.span)
      case _ => representation

  /** Returns the synthetic val defining `self` if it is hoisted. */
  def definition: Option[tpd.ValDef] =
    representation match
      case d: tpd.ValDef => Some(d)
      case _ => None

  /** Returns a tree representing the value of `self` along with its hoisted definition, if any. */
  def valueAndDefinition(using Context): (tpd.Tree, Option[tpd.ValDef]) =
    definition
      .map((d) => (tpd.Ident(d.namedType), Some(d)))
      .getOrElse((representation, None))

object PossiblyHoistedValue:

  /** Creates a value representing the `e`'s evaluation. */
  def apply(e: tpd.Tree)(using Context): PossiblyHoistedValue =
    if tpd.exprPurity(e) >= TreeInfo.Pure then
      new PossiblyHoistedValue(e)
    else
      new PossiblyHoistedValue(tpd.SyntheticValDef(TempResultName.fresh(), e))

/** The left-hand side of an assignment. */
private[typer] sealed abstract class LValue:

  /** Returns the local `val` definitions composing this lvalue. */
  def locals: List[tpd.ValDef]

  /** Returns a tree computing the assignment of `rhs` to this lvalue. */
  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree

end LValue

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
  */
private[typer] final case class ApplyLValue(
    function: ApplyLValue.Callee,
    arguments: List[PossiblyHoistedValue]
) extends LValue:

  val locals: List[tpd.ValDef] =
    function.locals ++ (arguments.flatMap { (v) => v.definition })

  def formAssignment(rhs: untpd.Tree)(using Context): untpd.Tree =
    val s = function.expanded
    val t = arguments.map((a) => untpd.TypedSplice(a.value)) :+ rhs
    untpd.Apply(s, t)

object ApplyLValue:

  /** The callee of a lvalue represented by a partial application. */
  sealed abstract class Callee:

    def expanded(using Context): untpd.Tree

    /** Returns the local `val` definitions composing this lvalue. */
    def locals: List[tpd.ValDef]

  object Callee:

    def apply(receiver: tpd.Tree)(using Context): Typed =
      Typed(PossiblyHoistedValue(receiver), None)

    def apply(receiver: tpd.Tree, member: Name)(using Context): Typed =
      Typed(PossiblyHoistedValue(receiver), Some(member))

    /** A function representing a lvalue. */
    final case class Typed(receiver: PossiblyHoistedValue, member: Option[Name]) extends Callee:

      def expanded(using Context): untpd.Tree =
        val s = untpd.TypedSplice(receiver.value)
        member.map((m) => untpd.Select(s, m)).getOrElse(s)

      def locals: List[tpd.ValDef] =
        receiver.definition.toList

    /** The untyped expression of a function representing a lvalue along with its captures. */
    final case class Untyped(value: untpd.Tree, locals: List[tpd.ValDef]) extends Callee:

      def expanded(using Context): untpd.Tree =
        value

  end Callee

end ApplyLValue
