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
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links._



class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger) {
    override def pageForClasslike(c: DClasslike): ClasslikePageNode = {
        val res = super.pageForClasslike(c)

        def renameCompanionObjectPage(objectPage: ClasslikePageNode): ClasslikePageNode = objectPage
        .modified(
            objectPage.getName + "$",
            objectPage.getContent,
            objectPage.getDri,
            objectPage.getEmbeddedResources,
            objectPage.getChildren
        )

        def addExtensionMethodPages(clazz: DClass, defContent: ClasslikePageNode): ClasslikePageNode = {
            val extensionPages = clazz.get(ClasslikeExtension).extensions.flatMap(_.extensions)
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
                val op1 = addExtensionMethodPages(clazz, res)
                val ext = clazz.get(ClasslikeExtension)
                if(ext.kind == dotty.dokka.Kind.Object && ext.companion.isDefined) renameCompanionObjectPage(op1)
                else op1
            case _ => res
        }
    }

    def insertCompanion(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        def companionContent(co: DRI): Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit] = builder => {
            group(builder)(builder => {
                    sourceSetDependentHint(builder)(
                        builder => {
                            group(builder)(builder => {
                                text(builder, "Companion ")()
                                clazz.get(ClasslikeExtension).kind match {
                                    case dotty.dokka.Kind.Object => link(builder, "class", co)()
                                    case _ => link(builder, "object", co)()
                                }
                                kotlin.Unit.INSTANCE
                            }, kind = ContentKind.Symbol)
                            kotlin.Unit.INSTANCE
                        }
                    )
                    kotlin.Unit.INSTANCE
                }, kind = ContentKind.Cover
            )
            kotlin.Unit.INSTANCE
        }

        clazz.get(ClasslikeExtension).companion.fold(defContent)(co => {
                    val addedContent = PageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentFor(clazz)(companionContent(co))
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

    def insertCustomExtensionTab(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val addedContent = PageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentFor(clazz)(builder => {
            groupingBlock(
                builder,
                "Extensions",
                clazz.get(ClasslikeExtension).extensions.map(e => e.extendedSymbol -> e.extensions).sortBy(_._2.size),
                (builder, receiver) => {
                    group(builder)(builder => {
                        builder.unaryPlus(builder.buildSignature(receiver))
                        kotlin.Unit.INSTANCE
                    })
                },
                (builder, elem) => {
                    link(builder, elem.getName, elem.getDri)(kind = ContentKind.Main)
                    sourceSetDependentHint(builder)( builder =>
                        {
                            contentForBrief(builder, elem)
                            builder.unaryPlus(builder.buildSignature(elem))
                            kotlin.Unit.INSTANCE
                        },
                        dri = Set(elem.getDri).asJava,
                        sourceSets = elem.getSourceSets,
                        kind = ContentKind.SourceSetDependentHint,
                        styles = Set().asJava
                    )
                    kotlin.Unit.INSTANCE
                }
            )(
                kind = ContentKind.Main,
                sourceSets = builder.getMainSourcesetData.asScala.toSet,
                styles = Set(),
                extra = PropertyContainer.Companion.empty().plus(SimpleAttr.Companion.header("Extensions")),
                false,
                true,
                Nil,
                false,
                true
            )
            kotlin.Unit.INSTANCE
        })
        val modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        modifyContentGroup(content, modifiedContent)
    }

    override def contentForClasslike(c: DClasslike): ContentGroup = {
        val defaultContent = super.contentForClasslike(c)
      
        c match{
            case clazz: DClass =>
                val op1 = insertCompanion(clazz, defaultContent)
                insertCustomExtensionTab(clazz, op1)
            case _ => defaultContent
        }
    }

    private def modifyContentGroup(originalContentNodeWithParents: Seq[ContentGroup], modifiedContentNode: ContentGroup): ContentGroup =
        originalContentNodeWithParents match {
            case head :: tail => tail match {
                case tailHead :: tailTail =>
                    val newChildren = tailHead.getChildren.asScala.map(c => if c != head then c else modifiedContentNode)
                    modifyContentGroup(
                        tailTail,
                        tailHead.copy(
                            newChildren.asJava,
                            tailHead.getDci,
                            tailHead.getSourceSets,
                            tailHead.getStyle,
                            tailHead.getExtra
                        )
                    )
                case _ => head
            }
            case _ => modifiedContentNode
        }

    private def getContentGroupWithParents(root: ContentGroup, condition: ContentGroup => Boolean): Seq[ContentGroup] = {
        def getFirstMatch(list: List[ContentNode]): Seq[ContentGroup] = list match {
            case head :: tail => head match {
                case g: ContentGroup => 
                    val res = getContentGroupWithParents(g, condition)
                    if(!res.isEmpty) res
                    else getFirstMatch(tail)
                case _ => getFirstMatch(tail)
            }
                
            case _ => Seq()
        }
        if(condition(root)) Seq(root)
        else {
            val res = getFirstMatch(root.getChildren.asScala.toList)
            if(!res.isEmpty) res ++ Seq(root)
            else Seq()
        }
    }

    private def groupingBlock[G, T <: Documentable](
        builder: PageContentBuilder#DocumentableContentBuilder,
        name: String,
        elements: List[T],
        groupingFunc: T => G,
        groupSplitterFunc: (PageContentBuilder#DocumentableContentBuilder, G) => Unit,
        elementFunc: (PageContentBuilder#DocumentableContentBuilder, T) => Unit
    )(
        kind: Kind = ContentKind.Main,
        sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData.asScala.toSet,
        styles: Set[Style] = builder.getMainStyles.asScala.toSet,
        extra: PropertyContainer[ContentNode] = builder.getMainExtra,
        renderWhenEmpty: Boolean = false,
        needsSorting: Boolean = true,
        headers: List[ContentGroup] = Nil,
        needsAnchors: Boolean = false,
        omitSplitterOnSingletons: Boolean = false
    ): Unit = {
        val grouped = elements.groupBy(groupingFunc).toList
        groupingBlock(
            builder, 
            name, 
            grouped, 
            groupSplitterFunc, 
            elementFunc
        )(
            kind, 
            sourceSets, 
            styles, 
            extra, 
            renderWhenEmpty, 
            needsSorting, 
            headers, 
            needsAnchors,
            omitSplitterOnSingletons
        )
    }

    private def groupingBlock[A, T <: Documentable, G <: List[(A, List[T])]](
        builder: PageContentBuilder#DocumentableContentBuilder,
        name: String,
        elements: G,
        groupSplitterFunc: (PageContentBuilder#DocumentableContentBuilder, A) => Unit,
        elementFunc: (PageContentBuilder#DocumentableContentBuilder, T) => Unit
    )(
        kind: Kind,
        sourceSets: Set[DokkaConfiguration$DokkaSourceSet],
        styles: Set[Style],
        extra: PropertyContainer[ContentNode],
        renderWhenEmpty: Boolean,
        needsSorting: Boolean,
        headers: List[ContentGroup],
        needsAnchors: Boolean,
        omitSplitterOnSingletons: Boolean
    ): Unit = if (renderWhenEmpty || !elements.isEmpty) {     
            header(builder, 3, name)(kind = kind)
            group(builder)(builder =>
            {
                elements.foreach((a, elems) => {
                    if(elems.size > 1 || !omitSplitterOnSingletons) groupSplitterFunc(builder, a)
                    builder.unaryPlus(
                        ContentTable(
                            headers.asJava,
                            (if(needsSorting) then elems.sortBy(_.getName) else elems)
                                .map( elem => {
                                    //TODO: There's problem with using extra property containers from Dokka in Scala
                                    //val newExtra = if(needsAnchors) then extra.plus(SymbolAnchorHint) else extra
                                    val newExtra = extra
                                    builder.buildGroup(Set(elem.getDri).asJava, elem.getSourceSets, kind, styles.asJava, newExtra, bdr => { 
                                        elementFunc(bdr, elem)
                                        kotlin.Unit.INSTANCE
                                    })
                                }).asJava,
                            DCI(builder.getMainDRI, kind),
                            sourceSets.asJava, styles.asJava, extra
                        )
                    )
                })
                kotlin.Unit.INSTANCE
            },
            styles = Set(ContentStyle.WithExtraAttributes),
            extra = extra
        )
    }

    private def contentForBrief(builder: PageContentBuilder#DocumentableContentBuilder, d: Documentable) = d.getSourceSets.asScala.foreach( ss =>
        d.getDocumentation.asScala.toMap.get(ss).flatMap(_.getChildren.asScala.headOption).map(_.getRoot).map( dt =>
            group(builder)(builder => 
                {
                    builder.comment(dt, ContentKind.Comment, builder.getMainSourcesetData, builder.getMainStyles, builder.getMainExtra)
                    kotlin.Unit.INSTANCE
                },
            sourceSets = Set(ss),
            kind = ContentKind.BriefComment
            )
        )
    )
        

}