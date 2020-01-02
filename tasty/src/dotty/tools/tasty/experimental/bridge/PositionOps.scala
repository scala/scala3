package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait PositionOps extends Core with

  given PositionedOps: (positioned: Positioned) extended with

    /** True if x's position shouldn't be reconstructed automatically from its initial span
     */
    def alwaysNeedsPos: Boolean = internal.Positioned_alwaysNeedsPos(positioned)

  given SourcePositionOps: (pos: SourcePosition) extended with
    def line: Int = internal.SourcePosition_line(pos)

  object Span with
    val empty: Span = internal.Span_empty
    val noSpan: Span = internal.Span_noSpan

  given SpanOps: (span: Span) extended with
    def coords: Long = internal.Span_coords(span)
    def isSynthetic: Boolean = internal.Span_isSynthetic(span)
    def toSynthetic: Span = internal.Span_toSynthetic(span)
    def start: Int = internal.Span_start(span)
    def end: Int = internal.Span_end(span)
    def pointDelta: Int = internal.Span_pointDelta(span)
    def exists: Boolean = internal.Span_exists(span)
