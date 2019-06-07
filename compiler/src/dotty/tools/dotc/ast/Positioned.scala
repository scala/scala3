package dotty.tools
package dotc
package ast

import util.Spans._
import util.{SourceFile, NoSource, SourcePosition}
import core.Contexts.Context
import core.Decorators._
import core.Flags.{JavaDefined, Extension}
import core.StdNames.nme
import annotation.constructorOnly
import annotation.internal.sharable
import reporting.Reporter

import java.io.{ PrintWriter }

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned(implicit @constructorOnly src: SourceFile) extends Product with Cloneable {

  private[this] var myUniqueId: Int = _
  private[this] var mySpan: Span = _

  /** A unique identifier. Among other things, used for determining the source file
   *  component of the position.
   */
  def uniqueId: Int = myUniqueId

  def uniqueId_=(id: Int): Unit = {
    def printTrace() = {
      println(s"Debug tree (id=${Positioned.debugId}) creation \n$this\n")
      Reporter.displayPrompt(Console.in, new PrintWriter(Console.err, true))
    }
    if (Positioned.debugId == id) printTrace()
    myUniqueId = id
  }

  /** The span part of the item's position */
  def span: Span = mySpan

  def span_=(span: Span): Unit = {
    mySpan = span
  }

  uniqueId = src.nextId
  span = envelope(src)

  def source: SourceFile = SourceFile.fromId(uniqueId)
  def sourcePos(implicit ctx: Context): SourcePosition = source.atSpan(span)

  /** A positioned item like this one with given `span`.
   *  If the positioned item is source-derived, a clone is returned.
   *  If the positioned item is synthetic, the position is updated
   *  destructively and the item itself is returned.
   */
  def withSpan(span: Span): this.type =
    if (span == mySpan) this
    else {
      val newpd: this.type =
        if (mySpan.isSynthetic) {
          if (!mySpan.exists && span.exists) {
            envelope(source, span.startPos) // fill in children spans
          }
          this
        }
        else cloneIn(source)
      newpd.span = span
      newpd
    }

  /** The union of startSpan and the spans of all positioned children that
   *  have the same source as this node, except that Inlined nodes only
   *  consider their `call` child.
   *
   *  Side effect: Any descendants without spans have but with the same source as this
   *  node have their span set to the end position of the envelope of all children to
   *  the left, or, if that one does not exist, to the start position of the envelope
   *  of all children to the right.
   */
  def envelope(src: SourceFile, startSpan: Span = NoSpan): Span = this match {
    case Trees.Inlined(call, _, _) =>
      call.span
    case _ =>
      def include(span: Span, x: Any): Span = x match {
        case p: Positioned =>
          if (p.source != src) span
          else if (p.span.exists) span.union(p.span)
          else if (span.exists) {
            if (span.end != MaxOffset)
              p.span = p.envelope(src, span.endPos)
            span
          }
          else // No span available to assign yet, signal this by returning a span with MaxOffset end
            Span(MaxOffset, MaxOffset)
        case m: untpd.Modifiers =>
          include(include(span, m.mods), m.annotations)
        case y :: ys =>
          include(include(span, y), ys)
        case _ => span
      }
      val limit = productArity
      def includeChildren(span: Span, n: Int): Span =
        if (n < limit) includeChildren(include(span, productElement(n)), n + 1)
        else span
      val span1 = includeChildren(startSpan, 0)
      val span2 =
        if (!span1.exists || span1.end != MaxOffset)
          span1
        else if (span1.start == MaxOffset)
          // No positioned child was found
          NoSpan
        else {
          ///println(s"revisit $uniqueId with $span1")
          // We have some children left whose span could not be assigned.
          // Go through it again with the known start position.
          includeChildren(span1.startPos, 0)
        }
      span2.toSynthetic
  }

  /** Clone this node but assign it a fresh id which marks it as a node in `file`. */
  def cloneIn(src: SourceFile): this.type = {
    val newpd: this.type = clone.asInstanceOf[this.type]
    newpd.uniqueId = src.nextId
    newpd
  }

  def contains(that: Positioned): Boolean = {
    def isParent(x: Any): Boolean = x match {
      case x: Positioned =>
        x.contains(that)
      case m: untpd.Modifiers =>
        m.mods.exists(isParent) || m.annotations.exists(isParent)
      case xs: List[_] =>
        xs.exists(isParent)
      case _ =>
        false
    }
    (this eq that) ||
      (this.span contains that.span) && {
        var n = productArity
        var found = false
        while (!found && n > 0) {
          n -= 1
          found = isParent(productElement(n))
        }
        found
      }
  }

  /** A hook that can be overridden if overlap checking in `checkPos` should be
   *  disabled for this node.
   */
  def disableOverlapChecks = false

  /** Check that all positioned items in this tree satisfy the following conditions:
   *  - Parent spans contain child spans
   *  - If item is a non-empty tree, it has a position
   */
  def checkPos(nonOverlapping: Boolean)(implicit ctx: Context): Unit = try {
    import untpd._
    var lastPositioned: Positioned = null
    var lastSpan = NoSpan
    def check(p: Any): Unit = p match {
      case p: Positioned =>
        assert(span contains p.span,
          i"""position error, parent span does not contain child span
             |parent      = $this # $uniqueId,
             |parent span = $span,
             |child       = $p # ${p.uniqueId},
             |child span  = ${p.span}""".stripMargin)
        p match {
          case tree: Tree if !tree.isEmpty =>
            assert(tree.span.exists,
              s"position error: position not set for $tree # ${tree.uniqueId}")
          case _ =>
        }
        if (nonOverlapping && !disableOverlapChecks) {
          this match {
            case _: XMLBlock =>
              // FIXME: Trees generated by the XML parser do not satisfy `checkPos`
            case _: WildcardFunction
            if lastPositioned.isInstanceOf[ValDef] && !p.isInstanceOf[ValDef] =>
              // ignore transition from last wildcard parameter to body
            case _ =>
              assert(!lastSpan.exists || !p.span.exists || lastSpan.end <= p.span.start,
                i"""position error, child positions overlap or in wrong order
                   |parent         = $this
                   |1st child      = $lastPositioned
                   |1st child span = $lastSpan
                   |2nd child      = $p
                   |2nd child span = ${p.span}""".stripMargin)
          }
          lastPositioned = p
          lastSpan = p.span
        }
        p.checkPos(nonOverlapping)
      case m: untpd.Modifiers =>
        m.annotations.foreach(check)
        m.mods.foreach(check)
      case xs: List[_] =>
        xs.foreach(check)
      case _ =>
    }
    this match {
      case tree: DefDef if tree.name == nme.CONSTRUCTOR && tree.mods.is(JavaDefined) =>
        // Special treatment for constructors coming from Java:
        // Leave out tparams, they are copied with wrong positions from parent class
        check(tree.mods)
        check(tree.vparamss)
      case tree: DefDef if tree.mods.is(Extension) =>
        tree.vparamss match {
          case vparams1 :: vparams2 :: rest if !isLeftAssoc(tree.name) =>
            check(vparams2)
            check(tree.tparams)
            check(vparams1)
            check(rest)
          case vparams1 :: rest =>
            check(vparams1)
            check(tree.tparams)
            check(rest)
          case _ =>
            check(tree.tparams)
        }
        check(tree.tpt)
        check(tree.rhs)
      case _ =>
        val end = productArity
        var n = 0
        while (n < end) {
          check(productElement(n))
          n += 1
        }
    }
  } catch {
    case ex: AssertionError =>
      println(i"error while checking $this")
      throw ex
  }
}

object Positioned {
  @sharable private[Positioned] var debugId = Int.MinValue

  def updateDebugPos(implicit ctx: Context): Unit = {
    debugId = ctx.settings.YdebugTreeWithId.value
  }
}
