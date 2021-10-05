package dotty.tools.scaladoc
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import org.jsoup.Jsoup
import scala.collection.JavaConverters._


case class LazyEntry(getKey: String, value: () => String) extends JMapEntry[String, Object]:
  lazy val getValue: Object = value()
  def setValue(x$0: Object): Object = ???

case class LoadedTemplate(
  templateFile: TemplateFile,
  children: List[LoadedTemplate],
  file: File):

  private def brief(ctx: StaticSiteContext): String =
    try
      val code = Jsoup.parse(resolveToHtml(ctx).code)
      Option(code.select("p").first()).fold("...")(_.outerHtml())
    catch
      case e: Throwable =>
        val msg = s"[ERROR] Unable to process brief for ${templateFile.file}"
        report.error(msg, templateFile.file, e)(using ctx.outerCtx)
        "..."

  def lazyTemplateProperties(ctx: StaticSiteContext): JMap[String, Object] = new java.util.AbstractMap[String, Object]():
    lazy val entrySet: JSet[JMapEntry[String, Object]] =
      val site = templateFile.settings.getOrElse("page", Map.empty).asInstanceOf[Map[String, Object]]
      site.asJava.entrySet() ++ JSet(
        LazyEntry("url", () => ctx.relativePath(LoadedTemplate.this).toString),
        LazyEntry("title", () => templateFile.title.name),
        LazyEntry("excerpt", () => brief(ctx))
      )

  def resolveToHtml(ctx: StaticSiteContext): ResolvedPage =
    val posts = children.map(_.lazyTemplateProperties(ctx))
    def getMap(key: String) = templateFile.settings.getOrElse(key, Map.empty).asInstanceOf[Map[String, Object]]

    val updatedSettings = templateFile.settings ++ ctx.projectWideProperties +
      ("site" -> (getMap("site") + ("posts" -> posts))) +
      ("page" -> (getMap("page") + ("title" -> templateFile.title.name)))

    templateFile.resolveInner(RenderingContext(updatedSettings, ctx.layouts))(using ctx)
