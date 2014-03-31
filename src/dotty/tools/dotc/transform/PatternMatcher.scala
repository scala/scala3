package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Constants._
import core.StdNames._
import core.transform.Erasure.isUnboundedGeneric
import typer.ErrorReporting._
import ast.Trees._

/** This transform eliminates patterns. Right now it's a dummy.
 *  Awaiting the real pattern matcher.
 */
class PatternMatcher extends TreeTransform {
  import ast.tpd._

  override def name: String = "patternMatcher"

  override def transformCaseDef(tree: CaseDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    cpy.CaseDef(tree, Literal(Constant("<eliminated pattern>")), tree.guard, tree.body)
}