package dotty.dokka
package site

import java.io.File
import java.nio.file.Files

import org.jetbrains.dokka.base.renderers.html.{NavigationNode, NavigationPage}
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.transformers.pages.PageTransformer
import org.jsoup.Jsoup
import scala.collection.JavaConverters._


case class LazyEntry(val getKey: String, value: () => String) extends JMapEntry[String, Object]:
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
    val updatedSettings = templateFile.settings + ("site" -> (site + ("posts" -> posts)))

    templateFile.resolveInner(RenderingContext(updatedSettings, ctx.layouts))
