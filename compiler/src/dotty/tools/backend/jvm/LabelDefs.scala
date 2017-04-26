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
 * Note that Label DefDefs can be only nested in Block, otherwise no one would
 * be able to call them Other DefDefs are eliminated
 */
class LabelDefs extends MiniPhaseTransform {
  def phaseName: String = "labelDef"

  val queue = new ArrayBuffer[Tree]()
  val beingAppended = new mutable.HashSet[Symbol]()
  var labelLevel = 0

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (tree.symbol is Flags.Label) tree
    else {
      collectLabelDefs.clear
      val newRhs = collectLabelDefs.transform(tree.rhs)
      var labelDefs = collectLabelDefs.labelDefs

      def putLabelDefsNearCallees = new TreeMap() {

        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          tree match {
            case t: Apply if labelDefs.contains(t.symbol) =>
              val labelDef = labelDefs(t.symbol)
              labelDefs -= t.symbol

              val labelDef2 = transform(labelDef)
              Block(labelDef2:: Nil, t)

            case _ => if (labelDefs.nonEmpty) super.transform(tree) else tree
          }
        }
      }

      val res = cpy.DefDef(tree)(rhs = putLabelDefsNearCallees.transform(newRhs))

      res
    }
  }

  object collectLabelDefs extends TreeMap() {

    // labelSymbol -> Defining tree
    val labelDefs = new mutable.HashMap[Symbol, Tree]()

    def clear = {
      labelDefs.clear()
    }

    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case t: Template => t
      case t: Block =>
        val r = super.transform(t)
        r match {
          case t: Block if t.stats.isEmpty => t.expr
          case _ => r
        }
      case t: DefDef =>
        assert(t.symbol is Flags.Label)
        val r = super.transform(tree)
        labelDefs(r.symbol) = r
        EmptyTree
      case t: Apply if t.symbol is Flags.Label =>
        val sym = t.symbol
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }
  }
}
