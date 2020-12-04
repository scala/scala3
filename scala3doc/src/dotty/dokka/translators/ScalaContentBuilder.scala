package dotty.dokka

import dotty.dokka.translators.FilterAttributes
import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder, PageContentBuilder$DocumentableContentBuilder}
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.doc._
import dotty.dokka.model.api.{Kind => _, Link => SLink, _}


case class DocumentableSubGroup(val title: Signature, val extensions: Seq[Documentable])

case class DocumentableGroup(name: Option[String | Documentable], documenables: Seq[Documentable | DocumentableSubGroup])

class ScalaPageContentBuilder(
  val commentsConverter: CommentsToContentConverter,
  val signatureProvider: SignatureProvider
)(using DocContext) {

  def contentForDRI(
    dri: DRI,
    sourceSets: Set[DokkaConfiguration$DokkaSourceSet],
    kind: Kind = ContentKind.Main,
    styles: Set[Style] = Set(),
    extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty(),
    buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
  ): ContentGroup = buildBlock(
      ScalaDocumentableContentBuilder(Set(dri), sourceSets, kind, styles, extra)
    ).buildContent()

  def contentForDRIs(
    dris: Set[DRI],
    sourceSets: Set[DokkaConfiguration$DokkaSourceSet],
    kind: Kind = ContentKind.Main,
    styles: Set[Style] = Set(),
    extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty(),
    buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
  ): ContentGroup = buildBlock(
      ScalaDocumentableContentBuilder(dris, sourceSets, kind, styles, extra)
    ).buildContent()

  def contentForDocumentable(
    d: Documentable,
    kind: Kind = ContentKind.Main,
    styles: Set[Style] = Set(),
    extra: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty(),
    buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
  ): ContentGroup = {
    buildBlock(
      ScalaDocumentableContentBuilder(Set(d.getDri), d.getSourceSets.asScala.toSet, kind, styles, extra)
    ).buildContent()
  }

  case class ScalaTableBuilder(
    val mainDRI: Set[DRI],
    val mainSourcesetData: Set[DokkaConfiguration$DokkaSourceSet],
    val mainKind: Kind,
    val mainStyles: Set[Style],
    val mainExtra: PropertyContainer[ContentNode],
    val cells: List[ContentGroup] = List()
  ) {
    private def addChild(c: ContentGroup) = copy(cells = cells :+ c)

    def cell(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = mainKind,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra)(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ): ScalaTableBuilder = addChild(contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock))

    def build() = cells
  }

  case class ScalaDivergentBuilder(
    val groupID: ContentDivergentGroup.GroupID,
    val mainDRI: Set[DRI],
    val mainKind: Kind,
    val mainStyles: Set[Style],
    val mainExtra: PropertyContainer[ContentNode],
    val implicitlySourceSetHinted: Boolean,
    val instances: List[ContentDivergentInstance] = List()
  ) {
    private def addChild(c: ContentDivergentInstance) = copy(instances = instances :+ c)

    def buildContent() = ContentDivergentGroup(
      instances.asJava,
      DCI(mainDRI.asJava, mainKind),
      mainStyles.asJava,
      mainExtra,
      groupID,
      implicitlySourceSetHinted
    )

    def instance(
      dri: Set[DRI],
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet],
      kind: Kind = mainKind,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDivergentInstanceBuilder => ScalaPageContentBuilder#ScalaDivergentInstanceBuilder
    ): ScalaDivergentBuilder = addChild(
      buildBlock(ScalaDivergentInstanceBuilder(dri, kind, sourceSets, styles, extra)).buildContent()
    )
  }

  case class ScalaDivergentInstanceBuilder(
    val mainDRI: Set[DRI],
    val mainKind: Kind,
    val mainSourcesetData: Set[DokkaConfiguration$DokkaSourceSet],
    val mainStyles: Set[Style],
    val mainExtra: PropertyContainer[ContentNode],
    val before: Option[ContentNode] = None,
    val divergent: ContentNode = null,
    val after: Option[ContentNode] = None
  ) {
    def buildContent() = ContentDivergentInstance(
      before.getOrElse(null),
      if divergent != null then divergent else throw IllegalStateException("Divergent part is mandatory"),
      after.getOrElse(null),
      DCI(mainDRI.asJava, mainKind),
      mainSourcesetData.toDisplay,
      mainStyles.asJava,
      mainExtra
    )

    def before(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = mainKind,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ): ScalaDivergentInstanceBuilder = copy(
      before = Some(contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock))
    )

    def divergent(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = ContentKind.Main,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ): ScalaDivergentInstanceBuilder = copy(
      divergent = contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock)
    )

    def after(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = ContentKind.Main,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ): ScalaDivergentInstanceBuilder = copy(
      after = Some(
        contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock)
      )
    )
  }


  case class ScalaDocumentableContentBuilder(
    val mainDRI: Set[DRI],
    val mainSourcesetData: Set[DokkaConfiguration$DokkaSourceSet],
    val mainKind: Kind,
    val mainStyles: Set[Style],
    val mainExtra: PropertyContainer[ContentNode],
    val children: List[ContentNode] = List()
  ) {

    def addChild(c: ContentNode) = copy(children = children :+ c)

    def addChildren(c: Seq[ContentNode]) = copy(children = children ++ c)

    def reset() = copy(children = Nil)

    def buildContent() = ContentGroup(
      children.asJava,
      DCI(mainDRI.asJava, mainKind),
      mainSourcesetData.toDisplay,
      mainStyles.asJava,
      mainExtra
    )

    def group(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = mainKind,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ): ScalaDocumentableContentBuilder = addChild(
      contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock)
    )

    def header(
      level: Int,
      text: String,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = p => p
    ): ScalaDocumentableContentBuilder = addChild(
        ContentHeader(
          level,
          contentForDRIs(
            mainDRI,
            sourceSets,
            kind,
            styles,
            extra plus SimpleAttr("anchor", "\\s".r.replaceAllIn(text, "").toLowerCase()),
            bdr => {buildBlock(bdr.text(text, kind = kind))}
          )
        )
      )

    def cover(
      text: String,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = p => p
    ): ScalaDocumentableContentBuilder = header(1, text, kind, sourceSets, styles, extra){buildBlock}

    def signature(d: Documentable) = addChildren(signatureProvider.signature(d).asScala.toList)

    private def buildSignature(d: Documentable, s: Signature) = signatureProvider.asInstanceOf[ScalaSignatureProvider].signature(d, s)

    def signature(d: Documentable, s: Signature) = addChild(buildSignature(d, s))

    def inlineSignature(d: Documentable, s: Signature) = addChildren(
      buildSignature(d, s).getChildren.asScala.toSeq
    )

    def defaultHeaders = List(
      contentForDRIs(
        dris = mainDRI,
        sourceSets = mainSourcesetData,
        buildBlock = bdr => {bdr.text("Name")}
      ),
      contentForDRIs(
        dris = mainDRI,
        sourceSets = mainSourcesetData,
        buildBlock = bdr => {bdr.text("Summary")}
      )
    )

    def table(
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra,
      headers: List[ContentGroup] = List.empty
      )(
      buildBlock: ScalaPageContentBuilder#ScalaTableBuilder => ScalaPageContentBuilder#ScalaTableBuilder
      ) = addChild(
      ContentTable(
        headers.asJava,
        buildBlock(ScalaTableBuilder(mainDRI, sourceSets, kind, styles, extra)).build().asJava,
        DCI(mainDRI.asJava, kind),
        sourceSets.toDisplay,
        styles.asJava,
        extra
      )
    )

    def text(
      text: String,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = addChild(
      buildText(text, kind, sourceSets, styles, extra)
    )

    private def buildText(
      text: String,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = ContentText(text, DCI(mainDRI.asJava, kind), sourceSets.toDisplay, styles.asJava, extra)


    def dotDiagram(
      diagram: HierarchyGraph,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = addChild(HierarchyGraphContentNode(diagram, DCI(mainDRI.asJava, kind), sourceSets.toDisplay.asScala.toSet, styles, extra))

    def groupingBlock[A, T <: Documentable, G <: List[(A, List[T])]](
      name: String,
      elements: G,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = Set(),
      extra: PropertyContainer[ContentNode] = mainExtra,
      renderWhenEmpty: Boolean = false,
      needsSorting: Boolean = true,
      headers: List[ContentGroup] = List(),
      needsAnchors: Boolean = true,
      omitSplitterOnSingletons: Boolean = true
    )(
      groupSplitterFunc: (ScalaPageContentBuilder#ScalaDocumentableContentBuilder, A) => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    )(
      elementFunc: (ScalaPageContentBuilder#ScalaDocumentableContentBuilder, T) => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = if (renderWhenEmpty || !elements.flatMap(_._2).isEmpty) {
      header(3, name, kind, styles = styles, extra = extra plus SimpleAttr.Companion.header(name))()
      .group(styles = Set(ContentStyle.WithExtraAttributes), extra = extra plus SimpleAttr.Companion.header(name)){ bdr =>
        elements.foldLeft(bdr){ (b, groupped) =>
          val (key, values) = groupped
          (if(values.size > 1 || (values.size == 1 && !omitSplitterOnSingletons)) b.group()(bd => groupSplitterFunc(bd, key)) else b)
          .table(kind = kind, headers = headers, styles = styles, extra = extra plus SimpleAttr.Companion.header(name)){ tablebdr =>
            values.foldLeft(tablebdr){ (tablebdr, elem) =>
              tablebdr.cell(Set(elem.getDri), elem.getSourceSets.asScala.toSet, kind, styles, extra){ cellbdr =>
                elementFunc(cellbdr, elem)
              }
            }
          }
        }
      }
    } else this

    def list[T](
      elements: Seq[T],
      prefix: String = "",
      suffix: String = "",
      separator: String = ", ",
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData
    )(
      elemOp: (ScalaPageContentBuilder#ScalaDocumentableContentBuilder, T) => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = if(!elements.isEmpty){
      val withPrefix = (if(!prefix.isEmpty) text(prefix, sourceSets = sourceSets) else this)
      val insertedElems = elements.dropRight(1).foldLeft[ScalaPageContentBuilder#ScalaDocumentableContentBuilder](withPrefix){ (bdr, elem) =>
        elemOp(bdr, elem).text(separator, sourceSets = sourceSets)
      }
      val withLast = elemOp(insertedElems, elements.last)
      if(!suffix.isEmpty) withLast.text(suffix, sourceSets = sourceSets) else withLast
    } else this

    def driLink(
      text: String,
      address: DRI,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = addChild(
      ContentDRILink(
        List(buildText(text, kind, sourceSets, styles, extra)).asJava,
        address,
        DCI(mainDRI.asJava, kind),
        sourceSets.toDisplay,
        JSet(),
        PropertyContainer.Companion.empty()
      )
    )

    def resolvedLink(
      text: String,
      address: String,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = addChild(
      ContentResolvedLink(
        List(buildText(text, kind, sourceSets, styles, extra)).asJava,
        address,
        DCI(mainDRI.asJava, kind),
        sourceSets.toDisplay,
        JSet(),
        PropertyContainer.Companion.empty()
      )
    )

    def linkWithContent(
      address: DRI,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = addChild(
      ContentDRILink(
        contentForDRIs(mainDRI, sourceSets, kind, styles, extra, buildBlock).getChildren,
        address,
        DCI(mainDRI.asJava, kind),
        sourceSets.toDisplay,
        JSet(),
        PropertyContainer.Companion.empty()
      )
    )

    def comment(
      docTag: DocTag,
      kind: Kind = ContentKind.Comment,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = addChild(
      contentForDRIs(mainDRI, sourceSets, kind, styles, extra, bdr => bdr.addChildren(
        rawComment(docTag, kind, sourceSets, styles, extra)
      ))
    )

    def rawComment(
      docTag: DocTag,
      kind: Kind = ContentKind.Comment,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra
    ) = commentsConverter.buildContent(
          docTag,
          DCI(mainDRI.asJava, kind),
          sourceSets.asJava,
          JSet(),
          PropertyContainer.Companion.empty()
        ).asScala.toSeq

    def divergentGroup(
      groupId: ContentDivergentGroup.GroupID,
      dri: Set[DRI] = mainDRI,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra,
      implicitlySourceSetHinted: Boolean = true
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDivergentBuilder => ScalaPageContentBuilder#ScalaDivergentBuilder
    ) = addChild(
      buildBlock(ScalaDivergentBuilder(groupId, dri, kind, styles, extra, implicitlySourceSetHinted)).buildContent()
    )

    def sourceSetDependentHint(
      dri: Set[DRI] = mainDRI,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      kind: Kind = ContentKind.Main,
      styles: Set[Style] = mainStyles,
      extra: PropertyContainer[ContentNode] = mainExtra,
    )(
      buildBlock: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = addChild(
      PlatformHintedContent(
        contentForDRIs(dri, sourceSets, kind, styles, extra, buildBlock),
        sourceSets.toDisplay
      )
    )

    type Self = ScalaPageContentBuilder#ScalaDocumentableContentBuilder

    def documentableTab(name: String)(children: DocumentableGroup*): Self =
      def buildSignature(d: Documentable) =
        ScalaSignatureProvider.rawSignature(d, InlineSignatureBuilder()).asInstanceOf[InlineSignatureBuilder]

      def buildAnnotations(d: Member) =
        InlineSignatureBuilder().annotationsBlock(d).asInstanceOf[InlineSignatureBuilder].names.reverse

      def documentableElement(documentable: Member): DocumentableElement =
        val docs = documentable.getDocumentation.asScala.values.headOption.flatMap(_.getChildren.asScala.headOption)
        val signatureBuilder = buildSignature(documentable)
        val originInfo = documentable.origin match {
          case Origin.ImplicitlyAddedBy(name, dri) => Signature("Implicitly added by ", SLink(name, dri))
          case Origin.ExtensionFrom(name, dri) => Signature("Extension method from ", SLink(name, dri))
          case Origin.ExportedFrom(name, dri) =>
            val signatureName: String | dotty.dokka.model.api.Link = dri match
              case Some(dri: DRI) => SLink(name, dri)
              case None => name
            Signature("Exported from ", signatureName)
          case Origin.Overrides(overridenMembers) => 
            def intersperse(xs: Seq[SLink]): Seq[(String | SLink)] = 
              xs.flatMap(Seq(_, " -> ")).dropRight(1).asInstanceOf[Seq[(String | SLink)]] // dropRight returns `Seq[Object]`
            Signature("Definition classes: ").join(Signature(intersperse(overridenMembers.map(SLink(_, _))):_*))
          case _ => Nil
        }
        val styles: Set[Style] = if documentable.deprecated.isDefined then Set(TextStyle.Strikethrough) else Set.empty
        DocumentableElement(
          buildAnnotations(documentable),
          signatureBuilder.preName.reverse,
          DocumentableNameWithStyles(documentable.getName, styles),
          signatureBuilder.names.reverse,
          docs.fold(Nil)(d => reset().rawComment(d.getRoot)),
          originInfo,
          FilterAttributes.attributesFor(documentable),
          asParams(documentable.getDri)
        )

      def element(e: Documentable | DocumentableSubGroup): DocumentableElement | DocumentableElementGroup = e match
        case e: Documentable => documentableElement(e)
        case e: DocumentableSubGroup =>
          DocumentableElementGroup(
            e.title,
            e.extensions.map(documentableElement),
            asParams(mainDRI)
          )

      if (children.forall(_.documenables.isEmpty)) this else
          header(3, name, mainKind,mainSourcesetData, mainStyles, mainExtra plus SimpleAttr.Companion.header(name))()
          .group(styles = Set(ContentStyle.WithExtraAttributes), extra = mainExtra plus SimpleAttr.Companion.header(name)){ bdr =>
            children.foldLeft(bdr){ (bdr, list) =>
              if list.documenables.isEmpty then bdr
              else
                val header = list.name match
                  case Some(o: Documentable) =>
                    buildSignature(o).names.reverse
                  case option =>
                    option.toSeq.map(_.toString)


                bdr.addChild(DocumentableList(header, list.documenables.map(element), asParams(mainDRI)))
            }
      }

    def documentableFilter() = addChild(DocumentableFilter(asParams(mainDRI)))

    def asParams(dri: DRI): ContentNodeParams = asParams(Set(dri))

    def asParams(dri: Set[DRI]): ContentNodeParams = ContentNodeParams(
      new DCI(dri.asJava, mainKind),
      mainSourcesetData.toDisplay,
      mainStyles,
      mainExtra
    )

    def divergentBlock[A, T <: Documentable, G <: List[(A, List[T])]](
      name: String,
      elements: G,
      kind: Kind = ContentKind.Main,
      sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = mainSourcesetData,
      styles: Set[Style] = Set(),
      extra: PropertyContainer[ContentNode] = mainExtra,
      renderWhenEmpty: Boolean = false,
      needsSorting: Boolean = true,
      headers: List[ContentGroup] = List(),
      needsAnchors: Boolean = true,
      omitSplitterOnSingletons: Boolean = true
    )(
      groupSplitterFunc: (ScalaPageContentBuilder#ScalaDocumentableContentBuilder, A) => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = if (renderWhenEmpty || !elements.flatMap(_._2).isEmpty) {
      header(3, name, kind, styles = styles, extra = extra plus SimpleAttr.Companion.header(name))()
      .group(styles = Set(ContentStyle.WithExtraAttributes), extra = extra plus SimpleAttr.Companion.header(name)){ bdr =>
        elements.foldLeft(bdr){ (b, groupped) =>
          val (key, values) = groupped
          (if(values.size > 1 || (values.size == 1 && !omitSplitterOnSingletons)) b.group()(bd => groupSplitterFunc(bd, key)) else b)
          .table(kind = kind, headers = headers, styles = styles, extra = extra plus SimpleAttr.Companion.header(name)){ tablebdr =>
            values.groupBy(_.getName).foldLeft(tablebdr){ case (tablebdr,(elemName, divergentElems)) => tablebdr
              .cell(
                dri = divergentElems.map(_.getDri).toSet,
                sourceSets = divergentElems.flatMap(_.getSourceSets.asScala).toSet,
                kind = kind
              ){ cellbdr => cellbdr
                .driLink(elemName, divergentElems.head.getDri, kind = ContentKind.Main)
                .divergentGroup(ContentDivergentGroup.GroupID(name)){ divBdr =>
                  divergentElems.foldLeft(divBdr){ (bdr, elem) =>
                    bdr.instance(Set(elem.getDri), elem.getSourceSets.asScala.toSet){ insBdr => insBdr
                      .before(){ befbdr => befbdr
                        .contentForBrief(elem)
                      }
                      .divergent(){ divDivBdr => divDivBdr
                        .group(){ gbdr => gbdr
                          .signature(elem)
                        }
                      }
                    }
                  }
                }
              }

            }
          }
        }
      }
    } else this

    def contentForBrief(d: Documentable): ScalaDocumentableContentBuilder =
      d.getDocumentation.asScala.foldLeft(this){ case (builder, (ss, docs)) =>
        docs.getChildren.asScala.headOption.map(_.getRoot) match {
          case Some(dt) => builder.group(sourceSets = Set(ss), kind = ContentKind.BriefComment){ bldr => bldr.comment(dt) }
          case None => builder
        }
      }

  }
}
