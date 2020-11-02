package dotty.dokka

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining._
import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder}
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder$DocumentableContentBuilder
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
import org.jetbrains.dokka.links.DRIKt.getParent
import dotty.dokka.model.api._
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.Link

type DocBuilder = ScalaPageContentBuilder#ScalaDocumentableContentBuilder

class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger):

    private val contentBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

    override def pageForModule(m: DModule): ModulePageNode = super.pageForModule(m)

    private def updatePageNameForMember(page: PageNode, p: Member) =
        val name = p.kind match
            case Kind.Extension(_) =>  s"extension_${page.getName}"
            case _ => page.getName

        page.modified(name, page.getChildren)

    private def pagesForMembers(p: Member): Seq[PageNode] =
        p.allMembers.filter(_.origin == Origin.DefinedWithin).collect {
            case f: DFunction => updatePageNameForMember(pageForFunction(f), f)
            case c: DClass => updatePageNameForMember(pageForDClass(c), c)
        }

    override def pageForPackage(p: DPackage): PackagePageNode =
        val originalPage = super.pageForPackage(p)
        val originalPages: Seq[PageNode] = originalPage.getChildren.asScala.toList
        val allPage: Seq[PageNode] = originalPages ++ pagesForMembers(p)
        originalPage.modified(p.getName, allPage.asJava)

    override def pageForClasslike(c: DClasslike): ClasslikePageNode = c match {
            case clazz: DClass => pageForDClass(clazz)
            case other => throw UnsupportedOperationException("Only DClass classlike is supported.")
        }

    def pageForDClass(c: DClass): ClasslikePageNode = {
        val constructors = c.getConstructors

        val ext = c.get(ClasslikeExtension)

        val name = if c.kind == Kind.Object && ext.companion.isDefined then c.getName + "$" else c.getName

        ClasslikePageNode(
            name,
            contentForClasslike(c),
            JSet(c.getDri),
            c,
            (constructors.asScala.map(pageForFunction) ++
            c.getClasslikes.asScala.map(pageForClasslike) ++
            c.getFunctions.asScala.map(pageForFunction) ++
            pagesForMembers(c)).asJava,
            List.empty.asJava
        )

    }

    override def pageForFunction(f: DFunction) = super.pageForFunction(f)

    override def contentForModule(m: DModule) = {
        def buildBlock = (builder: DocBuilder) => builder
            .group(kind = ContentKind.Cover) { gbuilder => gbuilder
                .cover(m.getName)()
                .descriptionIfNotEmpty(m)
            }
            .addChildren(contentForComments(m).asScala.toSeq)
            .groupingBlock(
                "Packages",
                List("" -> m.getPackages.asScala.toList),
                kind = ContentKind.Packages,
                sourceSets = m.getSourceSets.asScala.toSet
            )(
                (bdr, elem) => bdr
            ) { (bdr, elem) => bdr
                .driLink(elem.getName, elem.getDri)
            }

        contentBuilder.contentForDocumentable(m, buildBlock = buildBlock)
    }

    override def contentForPackage(p: DPackage) = {
        def buildBlock = (builder: DocBuilder) => builder
            .group(kind = ContentKind.Cover) { gbuilder => gbuilder
                .cover(p.getName)()
                .descriptionIfNotEmpty(p)
            }
            .documentableFilter()
            .group(styles = Set(ContentStyle.TabbedContent)) { b => b
                .contentForScope(p)
            }

        contentBuilder.contentForDocumentable(p, buildBlock = buildBlock)
    }

    override def contentForClasslike(c: DClasslike) = c match {
        case d: DClass => contentForClass(d)
        case other => throw UnsupportedOperationException("Only DClass classlike is supported.")
    }

    def contentForClass(c: DClass) = {
        def buildBlock = (builder: DocBuilder) => builder
            .group(kind = ContentKind.Cover, sourceSets = c.getSourceSets.asScala.toSet) { gbdr => gbdr
                .cover(c.getName)()
                .sourceSetDependentHint(Set(c.getDri), c.getSourceSets.asScala.toSet) { sbdr => sbdr
                    .signature(c)
                    .contentForDescription(c)
                }
            }
            .documentableFilter()
            .group(styles = Set(ContentStyle.TabbedContent)) { b => b
                .contentForScope(c)
                .contentForEnum(c)
                .contentForConstructors(c)
                .contentForTypesInfo(c)
            }
        contentBuilder.contentForDocumentable(c, buildBlock = buildBlock)
    }

    override def contentForMember(d: Documentable) = {
        def buildBlock = (builder: DocBuilder) => builder
            .group(kind = ContentKind.Cover){ bd => bd.cover(d.getName)() }
            .divergentGroup(
                ContentDivergentGroup.GroupID("member")
            ) { divbdr => divbdr
                .instance(Set(d.getDri), sourceSets = d.getSourceSets.asScala.toSet) { insbdr => insbdr
                    .before(){ bbdr => bbdr
                        .contentForDescription(d)
                        .contentForComments(d)
                    }
                    .divergent(kind = ContentKind.Symbol) { dbdr => dbdr
                        .signature(d)
                    }
                }
            }
        contentBuilder.contentForDocumentable(d, buildBlock = buildBlock)
    }

    override def contentForFunction(f: DFunction) = contentForMember(f)

    extension (b: DocBuilder):
        def descriptionIfNotEmpty(d: Documentable): DocBuilder = {
            val desc = contentForDescription(d).asScala.toSeq
            val res = if desc.isEmpty then b else b
                .sourceSetDependentHint(
                    Set(d.getDri),
                    d.getSourceSets.asScala.toSet,
                    kind = ContentKind.SourceSetDependentHint,
                    styles = Set(TextStyle.UnderCoverText)
                ) { sourceSetBuilder => sourceSetBuilder
                        .addChildren(desc)
                }
            res
        }

        def contentForComments(d: Documentable) = b

        def contentForDescription(d: Documentable) = {
            val specialTags = Set[Class[_]](classOf[Description])

            type SourceSet = DokkaConfiguration$DokkaSourceSet

            val tags: List[(SourceSet, TagWrapper)] =
                d.getDocumentation.asScala.toList.flatMap( (pd, doc) => doc.getChildren.asScala.map(pd -> _).toList )

            val platforms = d.getSourceSets.asScala.toSet

            val description = tags.collect{ case (pd, d: Description) => (pd, d) }.drop(1).groupBy(_(0)).map( (key, value) => key -> value.map(_(1)))

            /** Collect the key-value pairs from `iter` into a `Map` with a `cleanup` step,
              * keeping the original order of the pairs.
              */
            def collectInMap[K, E, V](
                iter: Iterator[(K, E)]
            )(
                cleanup: List[E] => V
            ): collection.Map[K, V] = {
                val lhm = mutable.LinkedHashMap.empty[K, ListBuffer[E]]
                iter.foreach { case (k, e) =>
                    lhm.updateWith(k) {
                        case None => Some(ListBuffer.empty.append(e))
                        case Some(buf) =>
                            buf.append(e)
                            Some(buf)
                    }
                }
                lhm.iterator.map { case (key, buf) => key -> cleanup(buf.result)}.to(mutable.LinkedHashMap)
            }

            val unnamedTags: collection.Map[(SourceSet, Class[_]), List[TagWrapper]] =
                collectInMap {
                    tags.iterator
                        .filterNot { t =>
                            t(1).isInstanceOf[NamedTagWrapper] || specialTags.contains(t(1).getClass)
                        }.map { t =>
                            (t(0), t(1).getClass) -> t(1)
                        }
                }(cleanup = identity)

            val namedTags: collection.Map[
                String,
                Either[
                    collection.Map[SourceSet, NamedTagWrapper],
                    collection.Map[(SourceSet, String), ScalaTagWrapper.NestedNamedTag],
                ],
            ] = {
                val grouped = collectInMap {
                    tags.iterator.collect {
                        case (sourcesets, n: NamedTagWrapper) =>
                            (n.getName, n.isInstanceOf[ScalaTagWrapper.NestedNamedTag]) -> (sourcesets, n)
                    }
                }(cleanup = identity)

                grouped.iterator.map {
                    case ((name, true), values) =>
                        val groupedValues =
                            values.iterator.map {
                                case (sourcesets, t) =>
                                    val tag = t.asInstanceOf[ScalaTagWrapper.NestedNamedTag]
                                    (sourcesets, tag.subname) -> tag
                            }.to(mutable.LinkedHashMap)
                        name -> Right(groupedValues)
                    case ((name, false), values) =>
                        name -> Left(values.to(mutable.LinkedHashMap))
                }.to(mutable.LinkedHashMap)
            }

            b.group(Set(d.getDri), styles = Set(TextStyle.Block, TableStyle.Borderless)) { bdr =>
                val b1 = description.foldLeft(bdr){
                    case (bdr, (key, value)) => bdr
                            .group(sourceSets = Set(key)){ gbdr =>
                                value.foldLeft(gbdr) { (gbdr, tag) => gbdr
                                    .comment(tag.getRoot)
                                }
                            }
                }

                b1.table(kind = ContentKind.Comment, styles = Set(TableStyle.DescriptionList)){ tbdr =>
                    val withUnnamedTags = unnamedTags.foldLeft(tbdr){ case (bdr, (key, value) ) => bdr
                        .cell(sourceSets = Set(key(0))){ b => b
                            .text(key(1).getSimpleName, styles = Set(TextStyle.Bold))
                        }
                        .cell(sourceSets = Set(key(0))) { b => b
                            .list(value, separator = ""){ (bdr, elem) => bdr
                                .comment(elem.getRoot)
                            }
                        }
                    }

                    val withNamedTags = namedTags.foldLeft(withUnnamedTags){
                        case (bdr, (key, Left(value))) =>
                            value.foldLeft(bdr){ case (bdr, (sourceSets, v)) => bdr
                                .cell(sourceSets = Set(sourceSets)){ b => b
                                    .text(key)
                                }
                                .cell(sourceSets = Set(sourceSets)){ b => b
                                    .comment(v.getRoot)
                                }
                            }
                        case (bdr, (key, Right(groupedValues))) => bdr
                            .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                .text(key)
                            }
                            .cell(sourceSets = d.getSourceSets.asScala.toSet)(_.table(kind = ContentKind.Comment, styles = Set(TableStyle.NestedDescriptionList)){ tbdr =>
                                groupedValues.foldLeft(tbdr){ case (bdr, ((sourceSets, _), v)) => bdr
                                    .cell(sourceSets = Set(sourceSets)){ b => b
                                        .comment(v.identTag)
                                    }
                                    .cell(sourceSets = Set(sourceSets)){ b => b
                                        .comment(v.descTag)
                                    }
                                }
                            })
                    }

                    val withCompanion = d match {
                        case d: DClass =>
                            val ext = d.get(ClasslikeExtension)
                            val co = ext.companion
                            co.fold(withNamedTags) { co => withNamedTags
                                .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                    .text("Companion")
                                }
                                .cell(sourceSets = d.getSourceSets.asScala.toSet){ b => b
                                    .driLink(
                                        d.kind match {
                                            case Kind.Object => "class"
                                            case _ => "object"
                                        },
                                        co
                                    )
                                }
                            }
                        case _ => withNamedTags
                    }

                    d match{
                        case d: (WithSources & WithExtraProperties[_]) if d.get(SourceLinks) != null && !d.get(SourceLinks).links.isEmpty => d.get(SourceLinks).links.foldLeft(withCompanion){
                            case (bdr, (sourceSet, link)) => bdr
                                    .cell(sourceSets = Set(sourceSet)){ b => b
                                        .text("Source")
                                    }
                                    .cell(sourceSets = Set(sourceSet)){ b => b
                                        .resolvedLink("(source)", link)
                                    }
                        }
                        case other => withCompanion
                    }
                }
            }
        }

        def contentForScope(s: Documentable & WithScope & WithExtraProperties[_]) =
            def groupExtensions(extensions: Seq[Member]): Seq[DocumentableSubGroup] =
                extensions.groupBy(_.kind).map {
                    case (Kind.Extension(on), members) =>
                        val signature = Signature(s"extension (${on.name}: ") join on.signature join Signature(")")
                        DocumentableSubGroup(signature, members.toSeq)
                    case other => sys.error(s"unexpected value: $other")
                }.toSeq


            val (definedMethods, inheritedMethods) = s.membersBy(_.kind == Kind.Def)
            val (definedFields, inheritedFiles) = s.membersBy(m => m.kind == Kind.Val || m.kind == Kind.Var)
            val (definedClasslikes, inheritedClasslikes) = s.membersBy(m => m.kind.isInstanceOf[Classlike])
            val (definedTypes, inheritedTypes) = s.membersBy(_.kind.isInstanceOf[Kind.Type])
            val (definedGivens, inheritedGives) = s.membersBy(_.kind.isInstanceOf[Kind.Given])
            val (definedExtensions, inheritedExtensions) = s.membersBy(_.kind.isInstanceOf[Kind.Extension])
            val (definedImplicits, inheritedImplicits) = s.membersBy(_.kind.isInstanceOf[Kind.Implicit])

            b
                .contentForComments(s)
                .documentableTab("Type members")(
                    DocumentableGroup(Some("Types"), definedTypes),
                    DocumentableGroup(Some("Classlikes"), definedClasslikes),
                    DocumentableGroup(Some("Inherited types"), inheritedTypes),
                    DocumentableGroup(Some("Inherited classlikes"), inheritedClasslikes)
                )
                .documentableTab("Methods")(
                    DocumentableGroup(Some("Defined methods"), definedMethods),
                    DocumentableGroup(Some("Inherited methods"),  inheritedMethods),
                )
                .documentableTab("Value members")(
                    DocumentableGroup(Some("Defined value members"), definedFields),
                    DocumentableGroup(Some("Inherited value members"), inheritedFiles)
                )
                .documentableTab("Givens")(
                    DocumentableGroup(Some("Defined givens"), definedGivens),
                    DocumentableGroup(Some("Inherited givens"), inheritedGives)
                )
                .documentableTab("Extensions")(
                    DocumentableGroup(Some("Defined extensions"), groupExtensions(definedExtensions)),
                    DocumentableGroup(Some("Inherited extensions"), groupExtensions(inheritedExtensions))
                )
                .documentableTab("Implicits")(
                    DocumentableGroup(Some("Defined implicits"), definedImplicits),
                    DocumentableGroup(Some("Inherited implicits"), inheritedImplicits)
                )


        def contentForEnum(c: DClass) =
            b.documentableTab("Enum entries")(
                DocumentableGroup(None, c.membersBy(_.kind == Kind.EnumCase)._1) // Enum entries cannot be inherited
            )


        def contentForConstructors(c: DClass) =
             b.documentableTab("Constructors")(
                DocumentableGroup(None, c.getConstructors.asScala.toList)
            )


        def contentForTypesInfo(c: DClass) =
            val supertypes = c.parents
            val subtypes = c.knownChildren
            val graph = MemberExtension.getFrom(c).map(_.graph)

            def contentForTypeLink(builder: DocBuilder, link: LinkToType): DocBuilder =
                builder.group(styles = Set(TextStyle.Paragraph)) { builder =>
                    link.signature.foldLeft(builder.text(link.kind.name).text(" ")){ (builder, sigElement) => sigElement match
                        case Link(name, dri) => builder.driLink(name, dri)
                        case str: String => builder.text(str)
                    }
                }

            val withSupertypes = if supertypes.isEmpty then b else
                b.header(2, "Linear supertypes")()
                    .group(
                        kind = ContentKind.Comment,
                        styles = Set(ContentStyle.WithExtraAttributes),
                        extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Linear supertypes")
                ){ gbdr => gbdr
                        .group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)){ grbdr => grbdr
                            .list(supertypes.toList, separator = "")(contentForTypeLink)
                        }
                    }

            val withSubtypes = if (subtypes.isEmpty) withSupertypes else
                withSupertypes.header(2, "Known subtypes")()
                    .group(
                        kind = ContentKind.Comment,
                        styles = Set(ContentStyle.WithExtraAttributes),
                        extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Known subtypes")
                    ) { _.group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)) {
                            _.list(subtypes.toList, separator="")(contentForTypeLink)
                        }
                    }

            graph.fold(withSubtypes) { graph =>
                if graph.edges.isEmpty then withSubtypes else
                    withSubtypes.header(2, "Type hierarchy")().group(
                        kind = ContentKind.Comment,
                        styles = Set(ContentStyle.WithExtraAttributes),
                        extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Type hierarchy")
                    ) { _.group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)) {
                            _.dotDiagram(graph)
                        }
                    }
            }
