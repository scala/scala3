package dotty.tools
package dottydoc
package staticsite

import scala.util.control.NonFatal

import dotc.util.SourceFile
import dotc.core.Contexts.Context
import dotc.util.Spans.Span
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
  Filter.registerFilter(new EscapeCSS)

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

          // mm.index is incorrect, let's compute the index manually
          // mm.line starts at 1, not 0
          val index = content.lineToOffset(mm.line-1) + mm.charPositionInLine
          ctx.error(
            if (unexpected == "EOF")
              s"unexpected end of file, expected $expected"
            else
              s"unexpected $unexpected, expected $expected",
            content atSpan Span(index)
          )

          None
        }
        case _ => {
            throw ex
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
  import scala.collection.mutable.HashMap

  final val TokenSymbols = HashMap(
    "TagStart" -> "{%",
    "TagEnd"   -> "%}",
    "OutStart" -> "{{",
    "OutEnd"   -> "}}",
    "Pipe"     -> "|",
    "DotDot"   -> "..",
    "Dot"      -> ".",
    "Eq"       -> "==",
    "EqSign"   -> "=",
    "Gt"       -> ">",
    "GtEq"     -> ">=",
    "Lt"       -> "<",
    "LtEq"     -> "<=",
    "Minus"    -> "-",
    "Col"      -> ":",
    "Comma"    -> ",",
    "OPar"     -> "(",
    "CPar"     -> ")",
    "OBr"      -> "[",
    "CBr"      -> "]",
    "QMark"    -> "?"
  ).mapValuesInPlace((k,v) => s"'$v' ($k)")

  def token(i: Int): String =
    if (i == -1) "EOF"
    else if (i >= LiquidParser.tokenNames.length) "non-existing token"
    else {
      val name = LiquidParser.tokenNames(i)
      TokenSymbols.getOrElse(name, name)
    }
}
