package dotty.dokka
package site

import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages.{ContentNode, DCI, Style}

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
