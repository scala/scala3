package dotty.tools
package dottydoc
package staticsite

import scala.util.control.NonFatal

import dotc.util.SourceFile
import dotc.core.Contexts.Context
import dotc.util.Positions.{ Position, NoPosition }
import util.syntax._

trait Template {
  def path: String
  def content: SourceFile
  def show: String = new String(content.content)
}

case class TemplateRenderingError(path: String, ex: Throwable)
extends Exception(s"error rendering $path, $ex")

case class Layout(path: String, content: SourceFile) extends Template

case class Include(path: String, content: SourceFile) extends Template

case class LiquidTemplate(path: String, content: SourceFile) extends Template with ResourceFinder {
  import scala.collection.JavaConverters._
  import dotc.printing.Highlighting._
  import liqp.Template
  import liqp.filters.Filter
  import liqp.parser.Flavor.JEKYLL
  import java.util.{ HashMap, Map => JMap }
  import filters._
  import tags._

  /** Register filters to static container */
  Filter.registerFilter(new Reverse)
  Filter.registerFilter(new First)
  Filter.registerFilter(new Json)

  // For some reason, liqp rejects a straight conversion using `.asJava`
  private def toJavaMap(map: Map[String, AnyRef]): HashMap[String, Object] =
    map.foldLeft(new HashMap[String, Object]()) { case (map, (k, v)) =>
      map.put(k, v)
      map
    }

  private def protectedRender(op: => String)(implicit ctx: Context) = try {
    Some(op)
  } catch {
    case NonFatal(ex) => {
      // TODO: when we reimplement the liquid parser, this can go away. For now
      // this is an OK approximation of what went wrong.
      if ((ex.getCause eq null) || ex.getMessage.contains("exceeded the max amount of time")) {
        ctx.docbase.error(
          "unknown error occurred in " +
          Blue(path).toString +
          ", most likely incorrect usage of tag"
        )
        None
      }
      else ex.getCause match {
        case mm: org.antlr.runtime.MismatchedTokenException => {
          val unexpected = LiquidTemplate.token(mm.getUnexpectedType)
          val expected = LiquidTemplate.token(mm.expecting)

          ctx.error(
            if (unexpected == "EOF")
              s"unexpected end of file, expected: '$expected'"
            else
              s"unexpected token '$unexpected', expected: '$expected'",
            content atPos Position(mm.index)
          )

          None
        }
        case ex => {
          if (true || ctx.settings.debug.value)
            throw ex

          None
        }
      }
    }
  }

  def render(params: Map[String, AnyRef], includes: Map[String, Include])(implicit ctx: Context): Option[String] =
    protectedRender {
      Template.parse(show, JEKYLL)
        .`with`(ResourceInclude(params, includes))
        .`with`(RenderReference(params))
        .`with`(RenderLink(params))
        .`with`(RenderTitle(params))
        .`with`(Docstring(params))
        .render(toJavaMap(params))
    }
}

object LiquidTemplate {
  import liqp.parser.LiquidParser

  private val _tokens: Map[String, String] = Map(
    "TagStart" -> "{%",
    "TagEnd"   -> "%}"
  )

  def token(i: Int): String =
    if (i == -1) "EOF"
    else if (i >= LiquidParser.tokenNames.length)
      "non-existing token"
    else _tokens
      .get(LiquidParser.tokenNames(i))
      .getOrElse(s"token  $i")
}
