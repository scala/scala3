/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc
package transform

import ast.tpd
import core.Contexts.Context
import core.Symbols.defn
import MegaPhase._

/** Rewrite `classOf` calls as follow:
 *
 *  For every primitive class C whose boxed class is called B:
 *    classOf[C]    -> B.TYPE
 *  For every non-primitive class D:
 *    classOf[D]    -> Literal(Constant(erasure(D)))
 */
class ClassOf extends MiniPhase {
  import tpd._

  override def phaseName: String = "classOf"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree =
    if (tree.symbol eq defn.Predef_classOf) {
      val targ = tree.args.head.tpe
      clsOf(targ).ensureConforms(tree.tpe).withPos(tree.pos)
    }
    else tree
}
