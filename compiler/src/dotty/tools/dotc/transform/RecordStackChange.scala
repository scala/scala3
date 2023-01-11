package dotty.tools.dotc
package transform

import MegaPhase.MiniPhase
import core.*
import Contexts.*
import Flags.Lazy
import NonLocalReturns.isNonLocalReturn

/** A trait shared between LiftTry and DropBreaks.
 *  Overrides prepare methods for trees that push to the stack before some of their elements
 *  are evaluated.
 */
trait RecordStackChange extends MiniPhase:
  import ast.tpd.*

  /** The method to call to record a stack change */
  protected def stackChange(using Context): Context

  override def prepareForApply(tree: Apply)(using Context): Context =
    stackChange

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if !tree.symbol.exists
       || tree.symbol.isSelfSym
       || tree.symbol.owner == ctx.owner.enclosingMethod
          && !tree.symbol.is(Lazy)
            // The current implementation wraps initializers of lazy vals in
            // calls to an initialize method, which means that a `try` in the
            // initializer needs to be lifted. Note that the new scheme proposed
            // in #6979 would avoid this.
    then ctx
    else stackChange

  override def prepareForAssign(tree: Assign)(using Context): Context =
    if (tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod) ctx
    else stackChange

  override def prepareForReturn(tree: Return)(using Context): Context =
    if (!isNonLocalReturn(tree)) ctx
    else stackChange