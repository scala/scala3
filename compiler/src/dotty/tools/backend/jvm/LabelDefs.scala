package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.Trees.Thicket
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.transform.MegaPhase._
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
import dotty.tools.io.{Directory, PlainDirectory, AbstractFile}
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
 * Verifies that each Label DefDef has only a single address to jump back and
 * reorders them such that they are not nested and this address is a
 * fall-through address for the JVM.
 *
 * ```scala
 * <label> def foo(i: Int) = {
 *   <label> def bar = 0
 *   <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 *   dough(i)
 * }
 *
 * foo(100)
 * ```
 *
 * will get rewritten to:
 *
 * ```scala
 * <label> def foo(i: Int) = dough(i)
 * <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 * <label> def bar = 2
 *   foo(100)
 * ```
 *
 * Proposed way to generate this pattern in backend is:
 *
 * ```scala
 * foo(100)
 * <jump foo>
 * <label> def foo(i: Int) = dough(i)
 * // <jump a>                           // unreachable
 * <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 * // <jump a>                           // unreachable
 * <label> def bar = 2
 * // <jump a>                           // unreachable
 * <asm point a>
 * ```
 *
 * Unreachable jumps will be eliminated by local dead code analysis.
 * After JVM is smart enough to remove next-line jumps
 *
 * Note that his phase Ychecking this phase required softening scoping rules
 * as it intentionally allowed to break scoping rules inside methods for labels.
 * This is modified by setting `labelsReordered` flag in Phases.
 *
 * @author Dmitry Petrashko
 */
class LabelDefs extends MiniPhase {
  def phaseName: String = "labelDef"

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    if (tree.symbol is Label) tree
    else {
      val labelDefs = collectLabelDefs(tree.rhs)

      def putLabelDefsNearCallees = new TreeMap() {
        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          tree match {
            case t: Apply if labelDefs.contains(t.symbol) =>
              val labelDef = labelDefs(t.symbol)
              labelDefs -= t.symbol
              val labelDef2 = cpy.DefDef(labelDef)(rhs = transform(labelDef.rhs))
              Block(labelDef2:: Nil, t)
            case t: DefDef =>
              assert(t.symbol is Label)
              EmptyTree
            case _ => if (labelDefs.nonEmpty) super.transform(tree) else tree
          }
        }
      }

      cpy.DefDef(tree)(rhs = putLabelDefsNearCallees.transform(tree.rhs))
    }
  }

  private def collectLabelDefs(tree: Tree)(implicit ctx: Context): mutable.HashMap[Symbol, DefDef] = {
    // labelSymbol -> Defining tree
    val labelDefs = new mutable.HashMap[Symbol, DefDef]()
    new TreeTraverser {
      override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
        case t: DefDef =>
          assert(t.symbol is Label)
          labelDefs(t.symbol) = t
          traverseChildren(t)
        case _ => traverseChildren(tree)
      }
    }.traverse(tree)
    labelDefs
  }
}
