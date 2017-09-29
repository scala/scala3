package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import NameKinds.OuterSelectName
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import util.Positions._
import Names._

import collection.mutable
import ResolveSuper._

import scala.collection.immutable.::

/** This phase rewrites outer selects `E.n_<outer>` which were introduced by
 *  inlining to outer paths.
 */
class ElimOuterSelect extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "elimOuterSelect"

  override def runsAfterGroupsOf = Set(classOf[ExplicitOuter])
    // ExplicitOuter needs to have run to completion before so that all classes
    // that need an outer accessor have one.

  /** Convert a selection of the form `qual.n_<outer>` to an outer path from `qual` of
   *  length `n`.
   */
  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) =
    tree.name match {
      case OuterSelectName(_, nhops) =>
        val SkolemType(tp) = tree.tpe
        ExplicitOuter.outer.path(start = tree.qualifier, count = nhops).ensureConforms(tp)
      case _ => tree
    }
}
