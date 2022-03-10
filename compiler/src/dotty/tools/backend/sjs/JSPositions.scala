package dotty.tools.backend.sjs

import scala.language.unsafeNulls

import java.net.{URI, URISyntaxException}

import dotty.tools.dotc.core._
import Contexts._

import dotty.tools.dotc.report

import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import org.scalajs.ir

/** Conversion utilities from dotty Positions to IR Positions. */
class JSPositions()(using Context) {
  import JSPositions._

  private val sourceURIMaps: List[URIMap] = {
    ctx.settings.scalajsMapSourceURI.value.flatMap { option =>
      val uris = option.split("->")
      if (uris.length != 1 && uris.length != 2) {
        report.error("-scalajs-mapSourceURI needs one or two URIs as argument (separated by '->').")
        Nil
      } else {
        try {
          val from = new URI(uris.head)
          val to = uris.lift(1).map(str => new URI(str))
          URIMap(from, to) :: Nil
        } catch {
          case e: URISyntaxException =>
            report.error(s"${e.getInput} is not a valid URI")
            Nil
        }
      }
    }
  }

  private def sourceAndSpan2irPos(source: SourceFile, span: Span): ir.Position = {
    if (!span.exists) ir.Position.NoPosition
    else {
      // dotty positions and IR positions are both 0-based
      val irSource = span2irPosCache.toIRSource(source)
      val point = span.point
      val line = source.offsetToLine(point)
      val column = source.column(point)
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

  private object span2irPosCache { // scalastyle:ignore
    import dotty.tools.dotc.util._

    private var lastDotcSource: SourceFile = null
    private var lastIRSource: ir.Position.SourceFile = null

    def toIRSource(dotcSource: SourceFile): ir.Position.SourceFile = {
      if (dotcSource != lastDotcSource) {
        lastIRSource = convert(dotcSource)
        lastDotcSource = dotcSource
      }
      lastIRSource
    }

    private def convert(dotcSource: SourceFile): ir.Position.SourceFile = {
      dotcSource.file.file match {
        case null =>
          new java.net.URI(
              "virtualfile",        // Pseudo-Scheme
              dotcSource.file.path, // Scheme specific part
              null                  // Fragment
          )
        case file =>
          val srcURI = file.toURI
          sourceURIMaps.collectFirst {
            case URIMap(from, to) if from.relativize(srcURI) != srcURI =>
              val relURI = from.relativize(srcURI)
              to.fold(relURI)(_.resolve(relURI))
          }.getOrElse(srcURI)
      }
    }
  }
}

object JSPositions {
  final case class URIMap(from: URI, to: Option[URI])
}
