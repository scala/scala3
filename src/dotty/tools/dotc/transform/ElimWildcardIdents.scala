package dotty.tools.dotc.transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer, MiniPhaseTransform}
import dotty.tools.dotc.ast.{untpd, tpd}
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.{Scopes, Flags}
import dotty.tools.dotc.core.Symbols.NoSymbol
import scala.annotation.tailrec
import dotty.tools.dotc.core._
import Symbols._
import scala.Some
import dotty.tools.dotc.transform.TreeTransforms.{NXTransformations, TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable
import dotty.tools.dotc.core.Names.Name
import NameOps._
import Types._
import scala.collection.SortedSet
import Decorators._
import StdNames._
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.config.JavaPlatform

/**
 * Replace Ident("_") in tree with default values of corresponding type:
 *   numerics: `0`
 *   booleans: `false`
 *   classes: `null`
 */
class ElimWildcardIdents extends MiniPhaseTransform {
  import tpd._
  def phaseName: String = "elimWildcardIdents"


  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    def stripBlocks(arg: Tree): Tree =  arg match {
      case b: Block if b.stats.isEmpty => stripBlocks(b.expr)
      case _ => arg
    }
    val b = stripBlocks(tree.rhs)
    b match {
      case x: Ident if (x.name == nme.WILDCARD && x.symbol.isClass) =>
        tpd.DefDef(tree.symbol.asTerm, tpd.initValue(x.tpe))
      case _ => tree
    }
  }

}
