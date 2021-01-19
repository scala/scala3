package dotty.dokka
package site

import java.io.File
import java.nio.file.Files

import org.jetbrains.dokka.base.renderers.html.{NavigationNode, NavigationPage}
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.transformers.pages.PageTransformer

case class StaticPageNode(
                           template: TemplateFile,
                           override val getName: String,
                           override val getContent: ContentNode,
                           override val getDri: JSet[DRI],
                           override val getEmbeddedResources: JList[String],
                           override val getChildren: JList[PageNode],
                         ) extends ContentPage:
  override def getDocumentable: Documentable = null

  def title(): String = template.title
  def hasFrame(): Boolean = template.hasFrame

  def dri = getDri.iterator.next()

  override def modified(
    name: String,
    content: ContentNode,
    dri: JSet[DRI],
    embeddedResources: JList[String],
    children: JList[_ <: PageNode]): ContentPage =
      copy(template, name, content, dri, embeddedResources, children.asInstanceOf[JList[PageNode]])

  override def modified(name: String, children: JList[_ <: PageNode]): PageNode =
    copy(getName = name, getChildren = children.asInstanceOf[JList[PageNode]])

  def resources(): List[String] = getContent match
    case p: PartiallyRenderedContent => p.resolved.resources
    case _ => Nil
