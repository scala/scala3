package dotty.dokka
package site

import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages.{ContentNode, DCI, Style}
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import com.vladsch.flexmark.convert.html.FlexmarkHtmlParser
import org.jsoup.Jsoup
import scala.collection.JavaConverters._

case class PartiallyRenderedContent(
  template: TemplateFile,
  context: StaticSiteContext,
  override val getChildren: JList[ContentNode],
  override val getDci: DCI,
  override val getSourceSets: JSet[DisplaySourceSet],
  override val getStyle: JSet[Style] = JSet(),
  override val getExtra: PropertyContainer[ContentNode] = new PropertyContainer(JMap())
) extends ContentNode:
  override def hasAnyContent(): Boolean = true

  override def withNewExtras(newExtras: PropertyContainer[ContentNode]): ContentNode =
    copy(getExtra = newExtras)

  override def withSourceSets(sourceSets: JSet[DisplaySourceSet]): ContentNode =
    copy(getSourceSets = sourceSets)

  lazy val resolved = template.resolveToHtml(context)

  def procsesHtml(linkTo: String => String): String =
    val document = Jsoup.parse(resolved.code)
    document.select("a").forEach(element => element.attr("href", linkTo(element.attr("href"))))// forrach does not work here
    document.outerHtml()
