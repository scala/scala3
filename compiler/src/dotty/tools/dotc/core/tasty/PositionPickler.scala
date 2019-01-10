package dotty.tools
package dotc
package core
package tasty

import ast._
import ast.Trees._
import ast.Trees.WithLazyField
import util.{SourceFile, NoSource}
import core._
import Contexts._, Symbols._, Annotations._, Decorators._
import collection.mutable
import TastyBuffer._
import util.Spans._
import TastyFormat.SOURCE

class PositionPickler(pickler: TastyPickler, addrOfTree: untpd.Tree => Option[Addr]) {
  val buf: TastyBuffer = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)
  import ast.tpd._

  private val pickledIndices = new mutable.BitSet

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean): Int = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)
  }

  /** The result type of `matchDegree`, which encodes the kind of match between
   *  the position of a Positioned item and the inferred position computed by Positioned#setInitialPos.
   */
  private type MatchDegree = Int
  private final val Unknown = 0         // Nothing known yet
  private final val StartMatch = 1      // We know that at least one element matches `start`
  private final val EndMatch = 2        // We know that at least one element matches `end`
  private final val SourceMatch = 4     // We know that the first element matches `source`
  private final val SpanMismatch = 8    // We know that the span is different from initialSpan
  private final val SourceMismatch = 16 // We know the source is different from the first element
  private final val SpanMatch = StartMatch | EndMatch
  private final val AllMatch = SpanMatch | SourceMatch

  def picklePositions(roots: List[Tree])(implicit ctx: Context): Unit = {
    var lastIndex = 0
    var lastSpan = Span(0, 0)
    def pickleDeltas(index: Int, span: Span) = {
      val addrDelta = index - lastIndex
      val startDelta = span.start - lastSpan.start
      val endDelta = span.end - lastSpan.end
      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0, !span.isSynthetic))
      if (startDelta != 0) buf.writeInt(startDelta)
      if (endDelta != 0) buf.writeInt(endDelta)
      if (!span.isSynthetic) buf.writeInt(span.pointDelta)
      lastIndex = index
      lastSpan = span

      pickledIndices += index
    }

    def pickleSource(source: SourceFile): Unit = {
      buf.writeInt(SOURCE)
      buf.writeInt(pickler.nameBuffer.nameIndex(source.pathName).index)
    }

    /** True if x's position shouldn't be reconstructed automatically from its initial span
     */
    def alwaysNeedsPos(x: Positioned) = x match {
      case
          // initialSpan is inaccurate for trees with lazy field
          _: WithLazyField[_]

          // A symbol is created before the corresponding tree is unpickled,
          // and its position cannot be changed afterwards.
          // so we cannot use the tree initialSpan to set the symbol position.
          // Instead, we always pickle the position of definitions.
          | _: Trees.DefTree[_]

          // package defs might be split into several Tasty files
          | _: Trees.PackageDef[_] => true
      case _ => false
    }

    //val msgs = new mutable.ListBuffer[String]

    /** The degree to which the source position of `x` matches the initial position computed
     *  from its elements.
     *  @pre The span of `x` exists
     */
    def matchDegree(x: Positioned): MatchDegree = {
      val src = x.source
      val start = x.span.start
      val end = x.span.end

      def checkElem(acc: MatchDegree, elem: Any): MatchDegree = elem match {
        case _: Trees.TypeTree[_] =>
          // TypeTrees contribute nothing since they are pickled as types
          acc
        case elem: Positioned =>
          val espan = elem.span
          if (!espan.exists) acc  // elements without position don't contribute
          else {
            val esrc = elem.source
            if (!esrc.exists) acc  // elements without source don't contribute
            else if (esrc `ne` src)
              if ((acc & SourceMatch) == 0) SourceMismatch // first element source is different -> no match
              else acc // subsequent elements with different source don't contribute
            else {
              var matches = acc | SourceMatch
              val estart = espan.start
              val eend = espan.end
              if (estart == start) matches |= StartMatch
              if (eend == end) matches |= EndMatch
              if (estart < start || eend > end) matches |= SpanMismatch
              matches
            }
          }
        case elem: List[_] =>
          checkElems(acc, elem)
        case m: untpd.Modifiers =>
          checkElems(checkElems(acc, m.mods), m.annotations)
        case _ =>
          acc
      }
      def checkElems(acc: MatchDegree, elems: List[Any]): MatchDegree = elems match {
        case elem :: elems1 => checkElems(checkElem(acc, elem), elems1)
        case nil => acc
      }

      val limit = x.relevantElemCount
      var n = 0
      var acc: MatchDegree = Unknown
      while (n < limit) {
        acc = checkElem(acc, x.productElement(n))
        n += 1
      }
      acc
    }

    def traverse(x: Any, current: SourceFile): Unit = x match {
      case x: untpd.Tree =>
        var sourceFile = current
        if (x.span.exists) {
          val mdegree = matchDegree(x)
          val sourceChange =
            mdegree == SourceMismatch ||           // source different from initial source, or
              (mdegree & SourceMatch) == 0 &&      // initial source unknown, and
              (x.source `ne` current)              // source different from context
          val needPos =
            (mdegree & SpanMismatch) != 0 ||       // initial span exceeds current in some direction, or
            (mdegree & SpanMatch) != SpanMatch ||  // initial span smaller than current, or
            sourceChange ||                        // source needs to be specified, or
            alwaysNeedsPos(x)                      // always needs position anyway
          if (needPos) {
            addrOfTree(x) match {
              case Some(addr) if !pickledIndices.contains(addr.index) || sourceChange =>
                // we currently do not share trees when unpickling, so if one path to a tree contains
                // a source change while another does not, we have to record the position of the tree twice
                // in order not to miss the source change. Test case is t3232a.scala.
                //println(i"pickling $x with $span at $addr")
                pickleDeltas(addr.index, x.span)
                if (x.source != current) {
                  pickleSource(x.source)
                  sourceFile = x.source
                }
              case _ =>
                //println(i"no address for $x")
            }
          }
        }
        //else if (x.span.exists) println(i"skipping $x")
        x match {
          case x: untpd.MemberDef @unchecked =>
            for (ann <- x.symbol.annotations) traverse(ann.tree, sourceFile)
          case _ =>
        }
        traverse(x.productIterator, sourceFile)
      case xs: TraversableOnce[_] =>
        xs.foreach(traverse(_, current))
      case x: Annotation =>
        traverse(x.tree, current)
      case _ =>
    }
    for (root <- roots) {
      traverse(root, NoSource)
    }
  }
}
