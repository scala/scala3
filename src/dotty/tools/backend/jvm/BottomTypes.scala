package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.Trees.Thicket
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, MiniPhase, MiniPhaseTransform}
import dotty.tools.dotc
import dotty.tools.dotc.backend.jvm.DottyPrimitives
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => JFile}

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.reflect.ClassTag
import scala.reflect.internal.util.WeakHashSet
import scala.reflect.io.{Directory, PlainDirectory, AbstractFile}
import scala.tools.asm.{ClassVisitor, FieldVisitor, MethodVisitor}
import scala.tools.nsc.backend.jvm.{BCodeHelpers, BackendInterface}
import dotty.tools.dotc.core._
import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import Phases._
import java.lang.AssertionError
import dotty.tools.dotc.util.Positions.Position
import Decorators._
import tpd._
import Flags._
import StdNames.nme

/**
  *  Ensures that tree does not contain type subsumptions where subsumed type is bottom type
  *  of our typesystem, but not the bottom type of JVM typesystem.
  */
class BottomTypes extends MiniPhaseTransform {
  def phaseName: String = "bottomTypes"


  def adaptBottom(treeOfBottomType: tpd.Tree, expectedType: Type)(implicit ctx: Context) = {
    if (defn.isBottomType(treeOfBottomType.tpe) && (treeOfBottomType.tpe ne expectedType))
      Erasure.Boxing.adaptToType(treeOfBottomType, expectedType)
    else treeOfBottomType
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val returnTp = tree.symbol.info.dealias.finalResultType
    cpy.DefDef(tree)(rhs = adaptBottom(tree.rhs, returnTp))
  }


  override def transformAssign(tree: tpd.Assign)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val returnTp = tree.lhs.symbol.info.dealias
    cpy.Assign(tree)(tree.lhs, adaptBottom(tree.rhs, returnTp))
  }


  override def transformTyped(tree: tpd.Typed)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    cpy.Typed(tree)(adaptBottom(tree.expr, tree.tpt.tpe), tree.tpt)
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val fun = tree.fun
    val newArgs: List[tpd.Tree] = tree.args.zip(fun.tpe.dealias.firstParamTypes).map(x => adaptBottom(x._1, x._2))
    val changeNeeded = tree.args != newArgs // cpy.Apply does not check if elements are the same,
                                            // it only does `eq` on lists as whole
    if (changeNeeded) cpy.Apply(tree)(fun = fun, args = newArgs)
    else tree
  }

  override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val returnTp = tree.symbol.info.dealias
    cpy.ValDef(tree)(rhs = adaptBottom(tree.rhs, returnTp))
  }
}
