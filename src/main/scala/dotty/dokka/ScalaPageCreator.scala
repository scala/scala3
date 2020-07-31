package dotty.dokka

import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder, PageContentBuilder$DocumentableContentBuilder}
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._


class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger) {
    override def pageForClasslike(c: DClasslike): ClasslikePageNode = {
        val res = super.pageForClasslike(c)
        def addCompanionObjectPage(co: DClasslike): ClasslikePageNode = {
            res.modified(
                res.getName,
                res.getContent,
                res.getDri,
                res.getEmbeddedResources,
                (res.getChildren.asScala ++ List(
                    ClasslikePageNode(
                        co.getName + "$",
                        contentForClasslike(co),
                        Set(co.getDri).asJava,
                        co,
                        (co.getClasslikes.asScala.map(pageForClasslike(_)) ++
                        co.getFunctions.asScala.map(pageForFunction(_))).asJava,
                        Nil.asJava
                    )
                )).asJava
            )
        }

        c match {
            case clazz: DClass => 
                clazz.get(ClasslikeExtension).companion.fold(res)(addCompanionObjectPage(_))
            case _ => res
        }
    }

    override def contentForClasslike(c: DClasslike): ContentGroup = {
        val defContent = super.contentForClasslike(c)
        def contentFunction(co: DClasslike): Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit] = builder => {
            group(builder)(builder => {
                    sourceSetDependentHint(builder)(
                        builder => {
                            builder.unaryPlus(builder.buildSignature(co))
                            kotlin.Unit.INSTANCE
                        }
                    )
                    kotlin.Unit.INSTANCE
                }, kind = ContentKind.Cover
            )
            kotlin.Unit.INSTANCE
        }
                    
        c match{
            case clazz: DClass =>
                clazz.get(ClasslikeExtension).companion.fold(defContent)(co => {
                    val addedContent = PageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentFor(clazz)(contentFunction(co))
                    val newChildren = List(defContent.getChildren.asScala.head) ++ List(addedContent) ++ defContent.getChildren.asScala.tail
                    ContentGroup(
                        newChildren.asJava,
                        defContent.getDci,
                        defContent.getSourceSets,
                        defContent.getStyle,
                        defContent.getExtra
                    )
                })
            case _ => defContent
        }
    }

}