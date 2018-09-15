package dotty.tools.dotc
package transform

import core._
import MegaPhase._
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
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import util.Positions._
import Names._

import collection.mutable
import ResolveSuper._

import scala.collection.immutable.::


/** This phase saves call-site `@tailrec` annotations as attachments.
 *
 *  Since erasure will come before the `tailrec` phase, it will erase the `@tailrec` annotations
 *  in the `Typed` nodes.
 */
class RecordTailRecCallSites extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "recordTailrecCallSites"

  override def transformTyped(tree: Typed)(implicit ctx: Context): Tree = {
    if (tree.tpt.tpe.hasAnnotation(defn.TailrecAnnot) && tree.expr.isInstanceOf[Apply])
      tree.expr.pushAttachment(TailRec.TailRecCallSiteKey, ())
    super.transformTyped(tree)
  }
}
