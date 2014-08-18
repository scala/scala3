package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import ast.Trees._

/** This phase rewrites idempotent expressions with constant types to Literals.
 *  The constant types are eliminated by erasure, so we need to keep
 *  the info about constantness in the trees.
 */
class Literalize extends MiniPhaseTransform {
  import ast.tpd._

  override def phaseName: String = "literalize"

  /** Note: Demanding idempotency instead of purity is strictly speaking too loose.
   *  Example
   *
   *    object O { final val x = 42; println("43") }
   *    O.x
   *
   *  Strictly speaking we can't replace `O.x` with `42`.  But this would make
   *  most expressions non-constant. Maybe we can change the spec to accept this
   *  kind of eliding behavior. Or else enforce true purity in the compiler.
   *  The choice will be affected by what we will do with `inline` and with
   *  Singleton type bounds (see SIP 23). Presumably
   *
   *     object O1 { val x: Singleton = 42; println("43") }
   *     object O2 { inline val x = 42; println("43") }
   *
   *  should behave differently.
   *
   *     O1.x  should have the same effect as   { println("43"; 42 }
   *
   *  whereas
   *
   *     O2.x = 42
   *
   *  Revisit this issue once we have implemented `inline`. Then we can demand
   *  purity of the prefix unless the selection goes to an inline val.
   */
  def literalize(tree: Tree)(implicit ctx: Context): Tree = tree.tpe match {
    case ConstantType(value) if isIdempotentExpr(tree) => Literal(value)
    case _ => tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    literalize(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    literalize(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    literalize(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    literalize(tree)
}