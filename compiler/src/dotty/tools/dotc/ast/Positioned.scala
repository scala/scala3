package dotty.tools
package dotc
package ast

import util.Spans.*
import util.{SourceFile, SourcePosition, SrcPos, WrappedSourceFile}
import WrappedSourceFile.MagicHeaderInfo, MagicHeaderInfo.*
import core.Contexts.*
import core.Decorators.*
import core.NameOps.*
import core.Flags.{JavaDefined, ExtensionMethod}
import core.StdNames.nme
import ast.Trees.mods
import annotation.constructorOnly
import annotation.internal.sharable

import scala.compiletime.uninitialized

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned(implicit @constructorOnly src: SourceFile) extends SrcPos, Product, Cloneable {
  import Positioned.{ids, nextId, debugId}

  private var mySpan: Span = uninitialized

  private var mySource: SourceFile = src

  /** A unique identifier in case -Yshow-tree-ids, or -Ydebug-tree-with-id
   *  is set, -1 otherwise.
   */
  def uniqueId: Int =
    if ids != null && ids.nn.containsKey(this) then ids.nn.get(this).nn else -1

  private def allocateId() =
    if ids != null then
      val ownId = nextId
      nextId += 1
      ids.nn.put(this: @unchecked, ownId)
      if ownId == debugId then
        println(s"Debug tree (id=$debugId) creation \n${this: @unchecked}\n")
        Thread.dumpStack()

  allocateId()

  /** The span part of the item's position */
  def span: Span = mySpan

  def span_=(span: Span): Unit =
    mySpan = span

  span = envelope(src)

  def source: SourceFile = mySource

  def sourcePos(using Context): SourcePosition =
    val info = WrappedSourceFile.locateMagicHeader(source)
    info match
      // This span is in user code
      case HasHeader(offset, originalFile)
        if span.exists && span.start >= offset =>
          originalFile.atSpan(span.shift(-offset))
      // Otherwise, return the source position in the wrapper code
      case _ => source.atSpan(span)

  /** This positioned item, widened to `SrcPos`. Used to make clear we only need the
   *  position, typically for error reporting.
   */
  final def srcPos: SrcPos = this

  /** A positioned item like this one with given `span`.
   *  If the positioned item is source-derived, a clone is returned.
   *  If the positioned item is synthetic, the position is updated
   *  destructively and the item itself is returned.
   */
  def withSpan(span: Span): this.type =
    if (span == mySpan) this
    else {
      val newpd: this.type =
        if !mySpan.exists then
          if span.exists then envelope(source, span.startPos) // fill in children spans
          this
        else
          cloneIn(source)
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
  def envelope(src: SourceFile, startSpan: Span = NoSpan): Span = (this: @unchecked) match {
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
        if (n < limit) includeChildren(include(span, productElement(n): @unchecked), n + 1)
        else span
      val span1 = includeChildren(startSpan, 0)
      val span2 =
        if (!span1.exists || span1.end != MaxOffset)
          span1
        else if (span1.start == MaxOffset)
          // No positioned child was found
          NoSpan
        else
          ///println(s"revisit $uniqueId with $span1")
          // We have some children left whose span could not be assigned.
          // Go through it again with the known start position.
          includeChildren(span1.startPos, 0)
      span2.toSynthetic
  }

  /** Clone this node but assign it a fresh id which marks it as a node in `file`. */
  def cloneIn(src: SourceFile): this.type = {
    val newpd: this.type = clone.asInstanceOf[this.type]
    newpd.allocateId()
    newpd.mySource = src
    newpd
  }

  def contains(that: Positioned): Boolean =
    def isParent(x: Any): Boolean = x match
      case x: Positioned =>
        x.contains(that)
      case m: untpd.Modifiers =>
        m.mods.exists(isParent) || m.annotations.exists(isParent)
      case xs: List[?] =>
        xs.exists(isParent)
      case _ =>
        false

    (this eq that)
    || this.span.contains(that.span)
      && {
        var n = productArity
        var found = false
        while !found && n > 0 do
          n -= 1
          found = isParent(productElement(n))
        found
      }
  end contains

  private class LastPosRef:
    var positioned: Positioned | Null = null
    var span = NoSpan

  /** Check that all positioned items in this tree satisfy the following conditions:
   *  - Parent spans contain child spans
   *  - If item is a non-empty tree, it has a position
   */
  def checkPos(nonOverlapping: Boolean)(using Context): Unit = try {
    import untpd.*
    val last = LastPosRef()
    def check(p: Any): Unit = p match {
      case p: Positioned =>
        assert(span.contains(p.span),
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
        if nonOverlapping then
          this match {
            case _: XMLBlock =>
              // FIXME: Trees generated by the XML parser do not satisfy `checkPos`
            case _: WildcardFunction
            if last.positioned.isInstanceOf[ValDef] && !p.isInstanceOf[ValDef] =>
              // ignore transition from last wildcard parameter to body
            case _ =>
              assert(!last.span.exists || !p.span.exists || last.span.end <= p.span.start,
                i"""position error, child positions overlap or in wrong order
                   |parent         = $this
                   |1st child      = ${last.positioned}
                   |1st child span = ${last.span}
                   |2nd child      = $p
                   |2nd child span = ${p.span}""".stripMargin)
          }
          last.positioned = p
          last.span = p.span
        p.checkPos(nonOverlapping)
      case m: untpd.Modifiers =>
        m.annotations.foreach(check)
        m.mods.foreach(check)
      case xs: List[?] =>
        xs.foreach(check)
      case _ =>
    }
    this match {
      case tree: DefDef if tree.name == nme.CONSTRUCTOR && tree.mods.is(JavaDefined) =>
        // Special treatment for constructors coming from Java:
        // Leave out leading type params, they are copied with wrong positions from parent class
        check(tree.mods)
        check(tree.trailingParamss)
      case tree: DefDef if tree.mods.is(ExtensionMethod) =>
        tree.paramss match
          case vparams1 :: vparams2 :: rest if tree.name.isRightAssocOperatorName =>
            // omit check for right-associatiove extension methods; their parameters were swapped
          case _ =>
            check(tree.paramss)
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
  }
  catch {
    case ex: AssertionError =>
      println(i"error while checking $this")
      throw ex
  }
}

object Positioned {
  @sharable private var debugId = Int.MinValue
  @sharable private var ids: java.util.WeakHashMap[Positioned, Int] | Null = null
  @sharable private var nextId: Int = 0

  def init(using Context): Unit =
    debugId = ctx.settings.YdebugTreeWithId.value
    if ids == null && ctx.settings.YshowTreeIds.value
       || debugId != ctx.settings.YdebugTreeWithId.default
    then
      ids = java.util.WeakHashMap()
}
