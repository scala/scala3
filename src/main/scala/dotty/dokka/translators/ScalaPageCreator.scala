package dotty.dokka

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining._
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
import org.jetbrains.dokka.links.DRIKt.getParent

class ScalaPageCreator(
    commentsToContentConverter: CommentsToContentConverter,
    signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DefaultPageCreator(commentsToContentConverter, signatureProvider, logger) {

    private val contentBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

    override def pageForModule(m: DModule): ModulePageNode = super.pageForModule(m)

    override def pageForPackage(p: DPackage): PackagePageNode = {
        val page = super.pageForPackage(p)

        val ext = Option(p.get(PackageExtension))
        val extensionPages = ext.fold(
                List.empty
            )(
                _.extensions.flatMap(_.extensions)
                .map(pageForFunction(_))
                .map(page =>
                    page.modified(
                        "extension_" + page.getName,
                        page.getChildren
                    )
                )
            )

        page.modified(
            page.getName,
            (page.getChildren.asScala ++ extensionPages).asJava
        )
    }

    override def pageForClasslike(c: DClasslike): ClasslikePageNode = c match {
            case clazz: DClass => pageForDClass(clazz)
            case other => throw UnsupportedOperationException("Only DClass classlike is supported.")
        }

    def pageForDClass(c: DClass): ClasslikePageNode = {
        val constructors = c.getConstructors

        val ext = c.get(ClasslikeExtension)

        val name = if ext.kind == dotty.dokka.Kind.Object && ext.companion.isDefined then c.getName + "$" else c.getName

        val extensionPages = ext.extensions.flatMap(_.extensions)
            .map(pageForFunction(_))
            .map(page =>
                page.modified(
                    "extension_" + page.getName,
                    page.getChildren
                )
            )
        val enumEntryPages = Option(c.get(EnumExtension)).map(_.enumEntries).collect{
            case c: DClasslike => pageForClasslike(c)
        }

        ClasslikePageNode(
            name,
            contentForClasslike(c),
            JSet(c.getDri),
            c,
            (constructors.asScala.map(pageForFunction) ++
            c.getClasslikes.asScala.map(pageForClasslike) ++
            c.getFunctions.asScala.map(pageForFunction) ++
            enumEntryPages ++ extensionPages).asJava,
            List.empty.asJava
        )

    }

    override def pageForFunction(f: DFunction) = super.pageForFunction(f)

    override def contentForModule(m: DModule) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
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
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover) { gbuilder => gbuilder
                .cover(p.getName)()
                .descriptionIfNotEmpty(p)
            }
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
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
            .group(kind = ContentKind.Cover, sourceSets = c.getSourceSets.asScala.toSet) { gbdr => gbdr
                .cover(c.getName)()
                .sourceSetDependentHint(Set(c.getDri), c.getSourceSets.asScala.toSet) { sbdr => sbdr
                    .signature(c)
                    .contentForDescription(c)
                }
            }
            .group(styles = Set(ContentStyle.TabbedContent)) { b => b
                .contentForScope(c)
                .contentForEnum(c)
                .contentForConstructors(c)
                .contentForTypesInfo(c)
            }
        contentBuilder.contentForDocumentable(c, buildBlock = buildBlock)
    }

    override def contentForMember(d: Documentable) = {
        def buildBlock = (builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) => builder
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

    extension (b: ScalaPageContentBuilder#ScalaDocumentableContentBuilder):
        def descriptionIfNotEmpty(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = {
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
                                        ext.kind match {
                                            case dotty.dokka.Kind.Object => "class"
                                            case _ => "object"
                                        },
                                        co
                                    )
                                }
                            }
                        case _ => withNamedTags
                    }

                    d match{
                        case d: (WithExpectActual & WithExtraProperties[Documentable]) if d.get(SourceLinks) != null && !d.get(SourceLinks).links.isEmpty => d.get(SourceLinks).links.foldLeft(withCompanion){
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

        def buildImplicitConversionInfo(by: Documentable) = 
            b.table(kind = ContentKind.BriefComment, styles = Set(TableStyle.DescriptionList)){ tbdr => tbdr
                .cell() { c => c
                    .text("Implicitly: ")
                }
                .cell() { c => c
                    .text("This member is added by an implicit conversion performed by ")
                    .driLink(by.getName, by.getDri)
                }
            }

        def buildInheritedExtensionInfo(by: Documentable) = 
            b.table(kind = ContentKind.BriefComment, styles = Set(TableStyle.DescriptionList)){ tbdr => tbdr
                .cell() { c => c
                    .text("Implicitly: ")
                }
                .cell() { c => c
                    .text("This member is added by an extension defined in ")
                    .driLink(by.getName, by.getDri)
                }
            }

        def contentForScope(
            s: Documentable & WithScope
        ) = {
            val (typeDefs, valDefs) = s.getProperties.asScala.toList.partition(_.get(PropertyExtension).kind == "type")
            val classes = s.getClasslikes.asScala.toList
            val inheritedDefinitions = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).inherited)
                case _ => None
            }
            val givens = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).givens)
                case p: DPackage => Option(p.get(PackageExtension)).map(_.givens)
                case _ => None
            }
            val extensions = s match {
                case c: DClass => Some(c.get(ClasslikeExtension).extensions)
                case p: DPackage => Option(p.get(PackageExtension)).map(_.extensions)
                case _ => None
            }
            val implicitMembers = s match {
                case c: DClass => Some(c.get(ImplicitMembers))
                case _ => None
            }

            val inheritedExtensions = implicitMembers.fold(Map.empty)(_.inheritedExtensions)
            val implicitMethods = implicitMembers.fold(Map.empty)(_.methods)
            val implicitInheritedMethods = implicitMembers.fold(Map.empty)(_.inheritedMethods)
            val implicitFields = implicitMembers.fold(Map.empty)(_.properties)
            val implicitInheritedFields = implicitMembers.fold(Map.empty)(_.inheritedProperties)
             
            val withoutExtensions = b
                .contentForComments(s)
                .divergentBlock(
                    "Type members",
                    List(
                        "Types" -> typeDefs, 
                        "Classlikes" -> classes, 
                        "Inherited types" -> inheritedDefinitions.fold(List.empty)(_.types),
                        "Inherited classlikes" -> inheritedDefinitions.fold(List.empty)(_.classlikes),
                    ),
                    kind = ContentKind.Classlikes,
                    omitSplitterOnSingletons = false
                )(
                    (bdr, elem) => bdr.header(3, elem)()
                )
                .groupingBlock(
                    "Methods",
                    List(
                        "Defined methods" -> (s.getFunctions.asScala ++ implicitMethods.keys ++ inheritedExtensions.keys).toList,
                        "Inherited methods" -> (inheritedDefinitions.fold(List.empty)(_.methods) ++ implicitInheritedMethods.keys.toList)
                    ),
                    kind = ContentKind.Functions,
                    omitSplitterOnSingletons = false
                )(
                    (builder, txt) => builder.header(3, txt)()
                ){ (bdr, elem) => bdr
                    .driLink(elem.getName, elem.getDri)
                    .sourceSetDependentHint(Set(elem.getDri), elem.getSourceSets.asScala.toSet, kind = ContentKind.SourceSetDependentHint) { sbdr => 
                        val withBrief = sbdr.contentForBrief(elem)
                        (if implicitMethods.contains(elem) then withBrief.buildImplicitConversionInfo(implicitMethods(elem))
                         else if implicitInheritedMethods.contains(elem) then withBrief.buildImplicitConversionInfo(implicitInheritedMethods(elem))
                         else if inheritedExtensions.contains(elem) then withBrief.buildInheritedExtensionInfo(inheritedExtensions(elem))
                         else withBrief)
                        .signature(elem)
                    }
                }
                .groupingBlock(
                    "Value members",
                    List(
                        "Defined value members" -> (valDefs ++ implicitFields.keys.toList),
                        "Inherited value members" -> (inheritedDefinitions.fold(List.empty)(_.fields) ++ implicitInheritedFields.keys.toList)
                    ),
                    kind = ContentKind.Properties,
                    omitSplitterOnSingletons = false,
                    sourceSets = s.getSourceSets.asScala.toSet
                )(
                    (bdr, elem) => bdr.header(3, elem)()
                ){ (bdr, elem) => bdr
                    .driLink(elem.getName, elem.getDri)
                    .sourceSetDependentHint(Set(elem.getDri), elem.getSourceSets.asScala.toSet, kind = ContentKind.SourceSetDependentHint) { sbdr => 
                        val withBrief = sbdr.contentForBrief(elem)
                        (if implicitFields.contains(elem) then withBrief.buildImplicitConversionInfo(implicitFields(elem))
                         else if implicitInheritedFields.contains(elem) then withBrief.buildImplicitConversionInfo(implicitInheritedFields(elem))
                         else withBrief)
                        .signature(elem)
                    }
                }
                .groupingBlock(
                    "Given",
                    List(
                        "Defined given" -> givens.getOrElse(List.empty),
                        "Inherited given" -> inheritedDefinitions.fold(List.empty)(_.givens)
                    ),
                    kind = ContentKind.Functions,
                    omitSplitterOnSingletons = false
                )(
                    (bdr, elem) => bdr.header(3, elem)()
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
                if(extensions.getOrElse(List.empty).isEmpty && inheritedDefinitions.fold(List.empty)(_.extensions).isEmpty) then withoutExtensions
                else withoutExtensions
                .header(2, "Extensions")()
                .group(styles = Set(ContentStyle.WithExtraAttributes), extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Extensions")){ gbdr => gbdr
                    .group(){ gdbdr => gbdr
                        .groupingBlock(
                            "Defined extensions",
                            extensions.getOrElse(List.empty).map(e => e.extendedSymbol -> e.extensions).sortBy(_._2.size),
                            omitSplitterOnSingletons = true,
                            kind = ContentKind.Extensions
                        )( (bdr, receiver) => bdr 
                            .group(){ grpbdr => grpbdr
                                .header(4, "Extension group")()
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
                        .groupingBlock(
                            "Inherited extensions",
                            inheritedDefinitions.fold(List.empty)(_.extensions).map(e => e.extendedSymbol -> e.extensions).sortBy(_._2.size),
                            omitSplitterOnSingletons = true,
                            kind = ContentKind.Extensions
                        )( (bdr, receiver) => bdr 
                            .group(){ grpbdr => grpbdr
                                .header(4, "Extension group")()
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
                    }
                }
        }

        def contentForEnum(
            c: DClass
        ) = b.groupingBlock(
            "Enum entries",
            List(() -> Option(c.get(EnumExtension)).fold(List.empty)(_.enumEntries.sortBy(_.getName).toList)),
            kind = ContentKind.Properties
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

        def contentForConstructors(
                c: DClass
            ) = b.groupingBlock(
                "Constructors",
                List("" -> c.getConstructors.asScala.toList),
                kind = ContentKind.Constructors
            )(
                (bdr, group) => bdr
            ){ (bdr, elem) => bdr
                    .driLink(elem.getName, elem.getDri)
                    .sourceSetDependentHint(Set(elem.getDri), elem.getSourceSets.asScala.toSet, kind = ContentKind.SourceSetDependentHint) { sbdr => sbdr
                        .contentForBrief(elem)
                        .signature(elem)
                    }
            }
    

        def contentForTypesInfo(c: DClass) = {
            val inheritanceInfo = c.get(InheritanceInfo)
            val supertypes = inheritanceInfo.parents
            val subtypes = inheritanceInfo.knownChildren
            def contentForType(
                bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
                b: DRI
            ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr
                .driLink(b.getClassNames, b)

            def contentForBound(
                bdr: ScalaPageContentBuilder#ScalaDocumentableContentBuilder,
                b: Bound
            ): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = b match {
                    case t: org.jetbrains.dokka.model.TypeConstructor => t.getProjections.asScala.foldLeft(bdr){
                        case (builder, p) => p match {
                            case text: UnresolvedBound => builder.text(text.getName)
                            case link: TypeParameter => builder.driLink(link.getName, link.getDri) 
                            case other => builder.text(s"TODO: $other")
                        }
                    }
                    case o => bdr.text(s"TODO: $o")
            }

            b.pipe { content =>
                if (!supertypes.isEmpty) {
                    content.header(2, "Linear supertypes")()
                    .group(
                        kind = ContentKind.Comment,
                        styles = Set(ContentStyle.WithExtraAttributes), 
                        extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Linear supertypes")
                    ) { _.group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)) { 
                            _.list(supertypes, separator = "") { (bdr, elem) => bdr
                                .group(styles = Set(TextStyle.Paragraph))(contentForBound(_, elem.bound))
                            }
                        }
                    }
                } else content
            }.pipe { content =>
                if (!subtypes.isEmpty) {
                    content.header(2, "Known subtypes")()
                    .group(
                        kind = ContentKind.Comment,
                        styles = Set(ContentStyle.WithExtraAttributes), 
                        extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Known subtypes")
                    ) { _.group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)) { 
                            _.list(subtypes.map(_.dri))(contentForType)
                        }
                    }
                } else content
            }.pipe { content =>
                content.header(2, "Type hierarchy")()
                .group(
                    kind = ContentKind.Comment,
                    styles = Set(ContentStyle.WithExtraAttributes), 
                    extra = PropertyContainer.Companion.empty plus SimpleAttr.Companion.header("Type hierarchy")
                ) { _.group(kind = ContentKind.Symbol, styles = Set(TextStyle.Monospace)) { 
                        _.dotDiagram(
                            HierarchyDiagramBuilder.build(DRIWithKind(c.getDri, c.get(ClasslikeExtension).kind),supertypes, subtypes)
                        )
                    }
                }
            }
        }

}
