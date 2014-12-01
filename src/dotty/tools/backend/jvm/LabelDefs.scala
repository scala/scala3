package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, MiniPhase, MiniPhaseTransform}
import dotty.tools.dotc.ast.tpd
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



  override def transformBlock(tree: tpd.Block)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    collectLabelDefs.clear
    val newStats = collectLabelDefs.transformStats(tree.stats)
    val newExpr = collectLabelDefs.transform(tree.expr)
    val labelCalls = collectLabelDefs.labelCalls
    val entryPoints = collectLabelDefs.parentLabelCalls
    val labelDefs = collectLabelDefs.labelDefs

    // make sure that for every label there's a single location it should return and single entry point
    // if theres already a location that it returns to that's a failure
    val disallowed = new mutable.HashMap[Symbol, Tree]()
    queue.sizeHint(labelCalls.size + entryPoints.size)
    def moveLabels(entryPoint: Tree): List[Tree] = {
      if((entryPoint.symbol is Flags.Label) && labelDefs.contains(entryPoint.symbol)) {
        val visitedNow = new mutable.HashMap[Symbol, Tree]()
        val treesToAppend = new ArrayBuffer[Tree]() // order matters. parents should go first
        queue.clear()

        var visited = 0
        queue += entryPoint
        while (visited < queue.size) {
          val owningLabelDefSym = queue(visited).symbol
          val owningLabelDef = labelDefs(owningLabelDefSym)
          for (call <- labelCalls(owningLabelDefSym))
            if (disallowed.contains(call.symbol)) {
              val oldCall = disallowed(call.symbol)
              ctx.error(s"Multiple return locations for Label $oldCall and $call", call.symbol.pos)
            } else {
              if ((!visitedNow.contains(call.symbol)) && labelDefs.contains(call.symbol)) {
                val df = labelDefs(call.symbol)
                visitedNow.put(call.symbol, labelDefs(call.symbol))
                queue += call
              }
            }
          if(!treesToAppend.contains(owningLabelDef))
            treesToAppend += owningLabelDef
          visited += 1
        }
        disallowed ++= visitedNow

        treesToAppend.toList
      } else Nil
    }

    cpy.Block(tree)(entryPoints.flatMap(moveLabels).toList ++ newStats, newExpr)

  }

  val collectLabelDefs = new TreeMap() {

    // label calls from this DefDef
    var parentLabelCalls: mutable.Set[Tree] = new mutable.HashSet[Tree]()
    var isInsideLabel = false
    var isInsideBlock = false

    def shouldMoveLabel = !isInsideBlock

    // labelSymbol -> Defining tree
    val labelDefs = new mutable.HashMap[Symbol, Tree]()
    // owner -> all calls by this owner
    val labelCalls = new mutable.HashMap[Symbol, mutable.Set[Tree]]()
    val labelCallCounts = new mutable.HashMap[Symbol, Int]()

    def clear = {
      parentLabelCalls.clear()
      labelDefs.clear()
      labelCalls.clear()
    }

    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case t: Template => t
      case t: Block => 
        val tmp = isInsideBlock
        isInsideBlock = true
        val r = super.transform(t)
        isInsideBlock = tmp
        r
      case t: DefDef =>
        assert(t.symbol is Flags.Label)
        val st = parentLabelCalls
        parentLabelCalls = new mutable.HashSet[Tree]()
        val tmp = isInsideLabel
        isInsideLabel = true
        val r = super.transform(tree)
        isInsideLabel = tmp
        labelCalls(r.symbol) = parentLabelCalls
        parentLabelCalls = st
        if(shouldMoveLabel) {
        labelDefs(r.symbol) = r
        EmptyTree
        } else r
      case t: Apply if t.symbol is Flags.Label =>
        parentLabelCalls = parentLabelCalls + t
        labelCallCounts.get(t.symbol)
        super.transform(tree)
      case _ =>
        super.transform(tree)

    }
  }
}
