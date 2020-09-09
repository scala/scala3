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
import org.jetbrains.dokka.model.doc._


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
                val pagesWithExtensions = addExtensionMethodPages(clazz, res)
                val ext = clazz.get(ClasslikeExtension)
                if(ext.kind == dotty.dokka.Kind.Object && ext.companion.isDefined) renameCompanionObjectPage(pagesWithExtensions)
                else pagesWithExtensions
            case _ => res
        }
    }

    def insertCompanion(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        def companionContent(co: DRI): ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = 
            bdr => bdr
                .group(kind = ContentKind.Cover){ grpbdr => grpbdr
                    .sourceSetDependentHint(){ srcsetbdr => srcsetbdr
                        .group(kind = ContentKind.Symbol){ gbdr => gbdr
                            .text("Companion ")
                            .driLink(
                                clazz.get(ClasslikeExtension).kind match {
                                    case dotty.dokka.Kind.Object => "class"
                                    case _ => "object"
                                },
                                co
                            )

                        }

                    }

                }
        clazz.get(ClasslikeExtension).companion.fold(defContent)(co => {
                    val addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(
                        clazz, 
                        buildBlock = companionContent(co)
                    )
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
        val addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(clazz, buildBlock = bdr =>
            bdr.groupingBlock(
                "Extensions",
                clazz.get(ClasslikeExtension).extensions.map(e => e.extendedSymbol -> e.extensions).sortBy(_._2.size),
            )( (bdr, receiver) => bdr 
                .group(){ grpbdr => grpbdr
                    .signature(receiver)
                }
            ){ (bdr, elem) => bdr
                .driLink(elem.getName, elem.getDri, kind = ContentKind.Main)
                .sourceSetDependentHint(
                    dri = Set(elem.getDri), 
                    sourceSets = elem.getSourceSets.asScala.toSet, 
                    kind = ContentKind.SourceSetDependentHint
                ){ srcsetbdr => srcsetbdr
                    .contentForBrief(elem)
                    .signature(elem)
                }
            }
        )

        val modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        modifyContentGroup(content, modifiedContent)
    }

    def insertEnumTab(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(clazz, buildBlock = bdr =>
            bdr.groupingBlock(
                "Entries",
                List(() -> clazz.get(EnumExtension).enumEntries.sortBy(_.getName).toList),
            )( (bdr, splitter) => bdr ){ (bdr, elem) => bdr
                .driLink(elem.getName, elem.getDri, kind = ContentKind.Main)
                .sourceSetDependentHint(
                    dri = Set(elem.getDri), 
                    sourceSets = elem.getSourceSets.asScala.toSet, 
                    kind = ContentKind.SourceSetDependentHint
                ){ srcsetbdr => srcsetbdr
                    .contentForBrief(elem)
                    .signature(elem)
                }
            }
        )

        val modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        modifyContentGroup(content, modifiedContent)
    }

    def insertInheritedMethods(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val addedContent = ScalaPageContentBuilder(
            commentsToContentConverter, 
            signatureProvider, 
            logger
        ).contentForDocumentable(clazz, buildBlock = builder => builder
            .divergentBlock(
                "Methods",
                List("Class methods" -> clazz.getFunctions.asScala.toList, "Inherited" -> clazz.get(ClasslikeExtension).inheritedMethods)
            )(
                (builder, txt) => builder.header(3, txt)()
            )
        )

        val filteredChildren = content(0).getChildren.asScala.map{ 
            case c: ContentGroup => c.copy(
                c.getChildren.asScala.filter(c => c.getDci.getKind != ContentKind.Functions).asJava,
                c.getDci,
                c.getSourceSets,
                c.getStyle,
                c.getExtra
            )
            case o => o
        }
        val modifiedContent = content(0).copy(
            (filteredChildren ++ List(addedContent)).asJava,            
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        modifyContentGroup(content, modifiedContent)
    }

    def insertGivenTab(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val givens = clazz.get(ClasslikeExtension).givens

        val addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(clazz, buildBlock = bdr =>
            bdr.groupingBlock(
                "Given",
                if(!givens.isEmpty) List(() -> givens.sortBy(_.getName).toList) else List.empty,
            )( (bdr, splitter) => bdr ){ (bdr, elem) => bdr
                .driLink(elem.getName, elem.getDri, kind = ContentKind.Main)
                .sourceSetDependentHint(
                    dri = Set(elem.getDri), 
                    sourceSets = elem.getSourceSets.asScala.toSet, 
                    kind = ContentKind.SourceSetDependentHint
                ){ srcsetbdr => srcsetbdr
                    .contentForBrief(elem)
                    .signature(elem)
                }
            }
        )

        val modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        modifyContentGroup(content, modifiedContent)
    }

    def insertLinearSupertypes(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val supertypes = clazz.get(InheritanceInfo).parents

        def contentForBound(
            bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
            b: Bound
        ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = b match{
                case t: org.jetbrains.dokka.model.TypeConstructor => t.getProjections.asScala.foldLeft(bdr){
                    case (builder, p) => p match {
                        case text: UnresolvedBound => builder.text(text.getName)
                        case link: TypeParameter => builder.driLink(link.getName, link.getDri) 
                        case other => builder.text(s"TODO: $other")
                    }
                }
                case o => bdr.text(s"TODO: $o")
            }
            

        def buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr => bdr
            .header(2, "Linear supertypes")()
            .group(
                styles = Set(ContentStyle.WithExtraAttributes), 
                extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Linear supertypes")
            ){ gbdr => gbdr
                .group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)){ grbdr => grbdr
                    .list(supertypes)(contentForBound)
                }
            }

        def addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(
            clazz, 
            buildBlock = buildBlock
        )

        def modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        if !supertypes.isEmpty then modifyContentGroup(content, modifiedContent) else defContent
    }

    def insertKnownSubclasses(clazz: DClass, defContent: ContentGroup): ContentGroup = {
        val content = getContentGroupWithParents(defContent, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
        val subtypes = clazz.get(InheritanceInfo).knownChildren

        def contentForType(
            bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
            b: DRI
        ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr
            .driLink(b.getClassNames, b)
            

        def buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr => bdr
            .header(2, "Known subtypes")()
            .group(
                styles = Set(ContentStyle.WithExtraAttributes), 
                extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Known subtypes")
            ){ gbdr => gbdr
                .group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)){ grbdr => grbdr
                    .list(subtypes)(contentForType)
                }
            }

        def addedContent = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger).contentForDocumentable(
            clazz, 
            buildBlock = buildBlock
        )

        def modifiedContent = content(0).copy(
            (content(0).getChildren.asScala ++ List(addedContent)).asJava,
            content(0).getDci,
            content(0).getSourceSets,
            content(0).getStyle,
            content(0).getExtra
        )
        if !subtypes.isEmpty then modifyContentGroup(content, modifiedContent) else defContent
    }

    override def contentForClasslike(c: DClasslike): ContentGroup = {
        val defaultContent = super.contentForClasslike(c)
      
        c match{
            case clazz: DClass =>
                val pageWithInheritedMethods = insertInheritedMethods(clazz, defaultContent)
                val pageWithCompanion = insertCompanion(clazz, pageWithInheritedMethods)
                // val pageWithCompanion = insertCompanion(clazz, defaultContent)
                val pageWithExtensionsTab = insertCustomExtensionTab(clazz, pageWithCompanion)
                val pageWithGivens = insertGivenTab(clazz, pageWithExtensionsTab)
                val pageWithInheritance = insertKnownSubclasses(clazz, insertLinearSupertypes(clazz, pageWithGivens))
                if clazz.get(ClasslikeExtension).kind == dotty.dokka.Kind.Enum then insertEnumTab(clazz, pageWithInheritance) else pageWithInheritance
            case _ => defaultContent
        }
    }


}