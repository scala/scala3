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
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions

class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger) {
    override def pageForClasslike(c: DClasslike): ClasslikePageNode = {
        val res = super.pageForClasslike(c)
        def addCompanionObjectPage(co: DClasslike, defContent: ClasslikePageNode): ClasslikePageNode = {
            val page = pageForClasslike(co)
            defContent.modified(
                defContent.getName,
                defContent.getContent,
                defContent.getDri,
                defContent.getEmbeddedResources,
                (defContent.getChildren.asScala ++ List(
                    page.modified(
                        page.getName + "$",
                        page.getContent,
                        page.getDri,
                        page.getEmbeddedResources,
                        page.getChildren
                    )
                )).asJava
            )
        }

        def addExtensionMethodPages(clazz: DClass, defContent: ClasslikePageNode): ClasslikePageNode = {
            val extensionPages = clazz.getExtra.getMap.asScala.values
                .collect { case e: CallableExtensions => e.getExtensions.asScala }
                .flatten
                .collect { case f: DFunction => f }
                .map(pageForFunction(_))
                .map(page =>
                    page.modified(
                        "extension_" + page.getName,
                        page.getContent,
                        page.getDri,
                        page.getEmbeddedResources,
                        page.getChildren
                    )
                )

            defContent.modified(
                defContent.getName,
                defContent.getContent,
                defContent.getDri,
                defContent.getEmbeddedResources,
                (defContent.getChildren.asScala ++ extensionPages).asJava
            )
        }
        
        c match {
            case clazz: DClass => 
                val op1 = clazz.get(ClasslikeExtension).companion.fold(res)(addCompanionObjectPage(_, res))
                val op2 = addExtensionMethodPages(clazz, op1)
                op2
            case _ => res
        }
    }

    def insertCompanionObject(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        def companionObjectContent(co: DClasslike): Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit] = builder => {
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

        clazz.get(ClasslikeExtension).companion.fold(defContent)(co => {
                    val addedContent = PageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentFor(clazz)(companionObjectContent(co))
                    val newChildren = List(defContent.getChildren.asScala.head) ++ List(addedContent) ++ defContent.getChildren.asScala.tail
                    ContentGroup(
                        newChildren.asJava,
                        defContent.getDci,
                        defContent.getSourceSets,
                        defContent.getStyle,
                        defContent.getExtra
                    )
        })
    }

    override def contentForClasslike(c: DClasslike): ContentGroup = {
        val defaultContent = super.contentForClasslike(c)
      
        c match{
            case clazz: DClass =>
                insertCompanionObject(clazz, defaultContent)
            case _ => defaultContent
        }
    }

}