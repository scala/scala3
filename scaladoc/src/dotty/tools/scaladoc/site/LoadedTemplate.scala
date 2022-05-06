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
  file: File,
  hidden: Boolean = false):

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
        LazyEntry("url", () => "/" ++ ctx.pathFromRoot(LoadedTemplate.this).toString),
        LazyEntry("title", () => templateFile.title.name),
        LazyEntry("excerpt", () => brief(ctx))
      )

  def resolveToHtml(ctx: StaticSiteContext): ResolvedPage =
    val subpages = children.filterNot(_.hidden).map(_.lazyTemplateProperties(ctx))
    def getMap(key: String) = templateFile.settings.getOrElse(key, Map.empty).asInstanceOf[Map[String, Object]]

    val sourceLinks = if !templateFile.file.exists() then Nil else
      // TODO (https://github.com/lampepfl/scala3doc/issues/240): configure source root
      // toRealPath is used to turn symlinks into proper paths
      val actualPath = Paths.get("").toAbsolutePath.relativize(templateFile.file.toPath.toRealPath())
      ctx.sourceLinks.pathTo(actualPath).map("viewSource" -> _ ) ++
        ctx.sourceLinks.pathTo(actualPath, operation = "edit").map("editSource" -> _)

    val updatedSettings = templateFile.settings ++ ctx.projectWideProperties +
      ("site" -> (getMap("site") + ("subpages" -> subpages))) + ("urls" -> sourceLinks.toMap) +
      ("page" -> (getMap("page") + ("title" -> templateFile.title.name)))

    templateFile.resolveInner(RenderingContext(updatedSettings, ctx.layouts))(using ctx)
