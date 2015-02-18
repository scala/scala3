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
 * Verifies that each Label DefDef has only a single address to jump back and
 * reorders them such that they are not nested and this address is a fall-through address for JVM
 *
 * ei such code
 *
 *
 * <label> def foo(i: Int) = {
 *   <label> def bar = 0
 *   <label> def dough(i: Int) = if(i == 0) bar else foo(i-1)
 *   dough(i)
 *   }
 *
 * foo(100)
 *
 * will get rewritten to
 *
 *                                                  \
 * <label> def foo(i: Int) = dough(i)
 * <label> def dough(i: Int) = if(i == 0) bar else foo(i-1)
 * <label> def bar = 2
 *   foo(100)
 *
 *   Proposed way to generate this pattern in backend is:
 *
 *  foo(100)
 *  <jump foo>
 *  <label> def foo(i: Int) = dough(i)
 *  // <jump a>                           // unreachable
 *  <label> def dough(i: Int) = if(i == 0) bar else foo(i-1)
 *  // <jump a>                           // unreachable
 *  <label> def bar = 2
 *  // <jump a>                           // unreachable
 *  <asm point a>
 *
 *    Unreachable jumps will be eliminated by local dead code analysis.
 *    After JVM is smart enough to remove next-line jumps
 *
 * Note that Label DefDefs can be only nested in Block, otherwise no one would be able to call them
 * Other DefDefs are eliminated
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
      val labelCalls = collectLabelDefs.labelCalls
      var entryPoints = collectLabelDefs.parentLabelCalls
      var labelDefs = collectLabelDefs.labelDefs
      var callCounts = collectLabelDefs.callCounts

      // make sure that for every label there's a single location it should return and single entry point
      // if theres already a location that it returns to that's a failure
      val disallowed = new mutable.HashMap[Symbol, Tree]()
      queue.sizeHint(labelCalls.size + entryPoints.size)

      def putLabelDefsNearCallees = new TreeMap() {

        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          tree match {
            case t: Apply if (entryPoints.contains(t)) =>
              entryPoints = entryPoints - t
              labelLevel = labelLevel + 1
              val r = Block(moveLabels(t), t)
              labelLevel = labelLevel - 1
              if(labelLevel == 0) beingAppended.clear()
              r
            case _ => if (entryPoints.nonEmpty && labelDefs.nonEmpty) super.transform(tree) else tree
          }

        }
      }

      def moveLabels(entryPoint: Apply): List[Tree] = {
        val entrySym = entryPoint.symbol
        if ((entrySym is Flags.Label) && labelDefs.contains(entrySym)) {
          val visitedNow = new mutable.HashMap[Symbol, Tree]()
          val treesToAppend = new ArrayBuffer[Tree]() // order matters. parents should go first
          treesToAppend += labelDefs(entrySym)
          queue.clear()

          var visited = 0
          queue += entryPoint
          while (visited < queue.size) {
            val owningLabelDefSym = queue(visited).symbol
            for (call <- labelCalls(owningLabelDefSym)) {
              val callSym = call.symbol
              if (!beingAppended.contains(callSym)) {
                if (disallowed.contains(callSym)) {
                  val oldCall = disallowed(callSym)
                  ctx.error(s"Multiple return locations for Label $oldCall and $call", callSym.pos)
                } else {
                  if ((!visitedNow.contains(callSym)) && labelDefs.contains(callSym)) {
                    val defTree = labelDefs(callSym)
                    visitedNow.put(callSym, defTree)
                    val callCount = callCounts(callSym)
                    if (callCount > 1) {
                      if (!treesToAppend.contains(defTree)) {
                        treesToAppend += defTree
                        queue += call

                      }
                    } else if (entryPoint.symbol ne callSym) entryPoints += call
                  }
                }
              }
            }

            visited += 1
          }
          beingAppended ++= treesToAppend.map(_.symbol)
          treesToAppend.toList.map(putLabelDefsNearCallees.transform)
        } else Nil
      }


      val res = cpy.DefDef(tree)(rhs = putLabelDefsNearCallees.transform(newRhs))

      res
    }
  }

  val collectLabelDefs = new TreeMap() {

    // label calls from this DefDef
    var parentLabelCalls: mutable.Set[Tree] = new mutable.HashSet[Tree]()
    var callCounts: mutable.Map[Symbol, Int] = new mutable.HashMap[Symbol, Int]().withDefaultValue(0)

    def shouldMoveLabel = true

    // labelSymbol -> Defining tree
    val labelDefs = new mutable.HashMap[Symbol, Tree]()
    // owner -> all calls by this owner
    val labelCalls = new mutable.HashMap[Symbol, mutable.Set[Tree]]()
    var owner: Symbol = null

    def clear = {
      parentLabelCalls.clear()
      labelDefs.clear()
      labelCalls.clear()
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

        val st = parentLabelCalls
        parentLabelCalls = new mutable.HashSet[Tree]()
        val symt = owner
        owner = t.symbol

        val r = super.transform(tree)

        owner = symt
        labelCalls(r.symbol) = parentLabelCalls
        parentLabelCalls = st

        if(shouldMoveLabel) {
          labelDefs(r.symbol) = r
          EmptyTree
        } else r
      case t: Apply if t.symbol is Flags.Label =>
        val sym = t.symbol
        parentLabelCalls = parentLabelCalls + t
        if(owner != sym) callCounts(sym) = callCounts(sym) + 1
        super.transform(tree)
      case _ =>
        super.transform(tree)

    }
  }
}
