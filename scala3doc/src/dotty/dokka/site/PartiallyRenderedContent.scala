package dotty.dokka
package site

import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages.{ContentNode, DCI, Style}
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import com.vladsch.flexmark.convert.html.FlexmarkHtmlParser
import org.jsoup.Jsoup
import scala.collection.JavaConverters._
import scala.util.Try
import java.net.URL

case class PartiallyRenderedContent(
  template: LoadedTemplate,
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

  def procsesHtml(linkTo: String => String, absoluteResource: String => String): String =
    val document = Jsoup.parse(resolved.code)
    document.select("a").forEach(element => element.attr("href", linkTo(element.attr("href"))))
    document.select("img").forEach { element =>
      val link = element.attr("src")
      Try(new URL(link)).getOrElse {
        if(link.startsWith("/")) element.attr("src", absoluteResource(link.drop(1)))
      }
    }// forrach does not work here
    document.outerHtml()


class A:
  def defInt: Int = 1
  def def1: 1 = 1
  val valInt: Int = 1
  val val1: 1 = 1
  var varInt: Int = 1
  var var1: 1 = 1

class B:
  val a = new A
  export a._

