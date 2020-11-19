package dotty.dokka
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import org.jetbrains.dokka.base.renderers.html.{NavigationNode, NavigationPage}
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.transformers.pages.PageTransformer
import org.jsoup.Jsoup
import scala.collection.JavaConverters._


case class LazyEntry(getKey: String, value: () => String) extends JMapEntry[String, Object]:
  lazy val getValue: Object = value()
  def setValue(x$0: Object): Object = ???

case class LoadedTemplate(templateFile: TemplateFile, children: List[LoadedTemplate], file: File):

  private def brief(ctx: StaticSiteContext): String =
    val code = Jsoup.parse(resolveToHtml(ctx).code)
    code.select("p").first().outerHtml()

  def lazyTemplateProperties(ctx: StaticSiteContext): JMap[String, Object] = new java.util.AbstractMap[String, Object]():
    def entrySet(): JSet[JMapEntry[String, Object]] =
      val site = templateFile.settings.getOrElse("page", Map.empty).asInstanceOf[Map[String, Object]]
      site.asJava.entrySet() ++ JSet(
        LazyEntry("url", () => ctx.relativePath(LoadedTemplate.this).toString),
        LazyEntry("title", () => templateFile.title),
        LazyEntry("excerpt", () => brief(ctx))
      )

  def resolveToHtml(ctx: StaticSiteContext): ResolvedPage =
    val posts = children.map(_.lazyTemplateProperties(ctx))
    val site = templateFile.settings.getOrElse("site", Map.empty).asInstanceOf[Map[String, Object]]
    val sourceLinks = if !file.exists() then Nil else
      // TODO (https://github.com/lampepfl/scala3doc/issues/240): configure source root
      // toRealPath is used to turn symlinks into proper paths
      val actualPath = Paths.get("").toAbsolutePath.relativize(file.toPath.toRealPath())
      ctx.sourceLinks.pathTo(actualPath).map("viewSource" -> _ ) ++
        ctx.sourceLinks.pathTo(actualPath, operation = "edit").map("editSource" -> _ )

    val updatedSettings = templateFile.settings ++ ctx.projectWideProperties +
      ("site" -> (site + ("posts" -> posts))) + ("urls" -> sourceLinks.toMap)

    templateFile.resolveInner(RenderingContext(updatedSettings, ctx.layouts))
