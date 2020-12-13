package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.base.transformers.pages.comments.{DocTagToContentConverter, CommentsToContentConverter}
import org.jetbrains.dokka.model.properties.{PropertyContainer, ExtraProperty, ExtraProperty$Key, MergeStrategy}
import java.util.{Set => JSet, List => JList}

object ScalaCommentToContentConverter extends DocTagToContentConverter {
  override def buildContent(
    docTag: DocTag,
    dci: DCI,
    sourceSets: JSet[? <: DokkaConfiguration$DokkaSourceSet],
    styles: JSet[? <: Style],
    extra: PropertyContainer[ContentNode]
  ): JList[ContentNode] = docTag match {
    case docTag: A =>
      val superRes = super.buildContent(docTag, dci, sourceSets, styles, extra).get(0)
      val res = superRes.withNewExtras(superRes.getExtra plus ExtraLinkAttributes(
        title = docTag.getParams.asScala.get("title")
      ))
      List(res).asJava

    case h: Html =>
      val children = h.getChildren
      require(children.size() == 1)
      require(children.get(0).isInstanceOf[Text])
      List(
        HtmlContentNode(
          children.get(0).asInstanceOf[Text].getBody,
          dci,
          sourceSets.asScala.toSet.toDisplay.asScala.toSet,
          styles.asScala.toSet
        )
      ).asJava
    case other => super.buildContent(other, dci, sourceSets, styles, extra)
  }

  case class ExtraLinkAttributes(title: Option[String]) extends ExtraProperty[ContentNode] {
    def getKey() = LinkAttributesKey
  }

  case object LinkAttributesKey extends ExtraProperty.Key[ContentNode, Null] {
    def mergeStrategyFor(left: Null, right: Null) = MergeStrategy.Fail(
      () => throw NotImplementedError(s"Property merging for $this is not implemented")
    ).asInstanceOf[MergeStrategy[ContentNode]]
  }
}
