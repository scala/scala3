package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._

import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import org.scalajs.ir

/** Conversion utilities from dotty Positions to IR Positions. */
class JSPositions()(implicit ctx: Context) {

  private def sourceAndSpan2irPos(source: SourceFile, span: Span): ir.Position = {
    if (!span.exists) ir.Position.NoPosition
    else {
      // dotty positions are 1-based but IR positions are 0-based
      val irSource = span2irPosCache.toIRSource(source)
      val point = span.point
      val line = source.offsetToLine(point) - 1
      val column = source.column(point) - 1
      ir.Position(irSource, line, column)
    }
  }

  /** Implicit conversion from dotty Span to ir.Position. */
  implicit def span2irPos(span: Span): ir.Position =
    sourceAndSpan2irPos(ctx.compilationUnit.source, span)

  /** Implicitly materializes an ir.Position from an implicit dotty Span. */
  implicit def implicitSpan2irPos(implicit span: Span): ir.Position =
    span2irPos(span)

  /** Implicitly materializes an ir.Position from an implicit dotty SourcePosition. */
  implicit def implicitSourcePos2irPos(implicit sourcePos: SourcePosition): ir.Position =
    sourceAndSpan2irPos(sourcePos.source, sourcePos.span)

  private[this] object span2irPosCache { // scalastyle:ignore
    import dotty.tools.dotc.util._

    private[this] var lastDotcSource: SourceFile = null
    private[this] var lastIRSource: ir.Position.SourceFile = null

    def toIRSource(dotcSource: SourceFile): ir.Position.SourceFile = {
      if (dotcSource != lastDotcSource) {
        lastIRSource = convert(dotcSource)
        lastDotcSource = dotcSource
      }
      lastIRSource
    }

    private[this] def convert(dotcSource: SourceFile): ir.Position.SourceFile = {
      dotcSource.file.file match {
        case null =>
          new java.net.URI(
              "virtualfile",        // Pseudo-Scheme
              dotcSource.file.path, // Scheme specific part
              null                  // Fragment
          )
        case file =>
          val srcURI = file.toURI
          def matches(pat: java.net.URI) = pat.relativize(srcURI) != srcURI

          // TODO
          /*scalaJSOpts.sourceURIMaps.collectFirst {
            case ScalaJSOptions.URIMap(from, to) if matches(from) =>
              val relURI = from.relativize(srcURI)
              to.fold(relURI)(_.resolve(relURI))
          } getOrElse*/ srcURI
      }
    }
  }
}
