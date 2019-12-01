package dotty.tools.tasty.experimental

import dotty.tools.tasty.TastyFormat.SOURCE
import dotty.tools.tasty.TastyBuffer
import TastyBuffer._

import collection.mutable

class PositionPickler[T <: Tasty with Singleton](val tasty: T)(val pickler: TastyPickler[tasty.type], addrOfTree: tasty.untpd.Tree => Addr) {
  import tasty.{_,given}
  val buf: TastyBuffer = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)

  private val pickledIndices = new mutable.BitSet

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean): Int = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)
  }

  def picklePositions(roots: List[Tree])(implicit ctx: Context): Unit = {
    var lastIndex = 0
    var lastSpan = Span.empty
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
      buf.writeInt(pickler.nameBuffer.nameIndex(source.path.toTermName).index)
    }

    def traverse(x: Any, current: SourceFile): Unit = x match {
      case x: untpd.Tree =>
        if (x.span.exists) {
          val addr = addrOfTree(x)
          if (addr != NoAddr) {
            if (x.source != current) {
              // we currently do not share trees when unpickling, so if one path to a tree contains
              // a source change while another does not, we have to record the position of the tree twice
              // in order not to miss the source change. Test case is t3232a.scala.
              pickleDeltas(addr.index, x.span)
              pickleSource(x.source)
            }
            else if (!pickledIndices.contains(addr.index) &&
                     (x.span.toSynthetic != x.envelope(x.source) || x.alwaysNeedsPos))
              pickleDeltas(addr.index, x.span)
          }
        }
        x match {
          case x: untpd.MemberDef => traverse(x.symbol.annotations, x.source)
          case _ =>
        }
        val limit = x.productArity
        var n = 0
        while (n < limit) {
          traverse(x.productElement(n), x.source)
          n += 1
        }
      case y :: ys =>
        traverse(y, current)
        traverse(ys, current)
      case x: Annotation =>
        traverse(x.tree, current)
      case _ =>
    }
    for (root <- roots)
      traverse(root, SourceFile.noSource)
  }
}
