package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.base.transformers.pages.comments.{DocTagToContentConverter, CommentsToContentConverter}
import java.util.{Set => JSet, List => JList}


object ScalaCommentToContentConverter extends CommentsToContentConverter:
    val defaultConverter = DocTagToContentConverter()
    override def buildContent(
        docTag: DocTag,
        dci: DCI,
        sourceSets: JSet[? <: DokkaConfiguration$DokkaSourceSet],
        styles: JSet[? <: Style],
        extra: PropertyContainer[ContentNode]
    ): JList[ContentNode] = docTag match {
        case h: Html => List(
            HtmlContentNode(
                h.getChildren.asScala.collect{case c: Text => c}.head.getBody,
                dci,
                sourceSets.asScala.toSet.toDisplay.asScala.toSet,
                styles.asScala.toSet
            )
        ).asJava
        case other => defaultConverter.buildContent(other, dci, sourceSets, styles, extra)
    }
