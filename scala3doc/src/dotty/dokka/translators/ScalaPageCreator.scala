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
import org.jetbrains.dokka.model.doc._
import dotty.dokka.model.api._
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.Link

type DocBuilder = ScalaPageContentBuilder#ScalaDocumentableContentBuilder

class ScalaPageCreator(
  commentsToContentConverter: CommentsToContentConverter,
  signatureProvider: SignatureProvider,
)(using ctx: DocContext)
  extends DefaultPageCreator(commentsToContentConverter, signatureProvider, ctx.logger):

  private val contentBuilder =
    ScalaPageContentBuilder(commentsToContentConverter, signatureProvider)

  override def pageForModule(m: DModule): ModulePageNode = super.pageForModule(m)

  private def pagesForMembers(member: Member): JList[PageNode] =
    val all = member
      .membersBy(_.kind.isInstanceOf[Classlike])
      .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
    all.map(pageForMember(_)).asJava

  override def pageForPackage(p: DPackage): PackagePageNode =
    PackagePageNode(
      p.name,
      contentForPackage(p),
      JSet(p.dri),
      p,
      pagesForMembers(p),
      JNil
    )

  def pageForMember(c: Member): ClasslikePageNode = {
    val name =
      if c.kind == Kind.Object && c.companion.isDefined then
        c.getName + "$"
      else c.getName

    // Hack, need our own page!
    ClasslikePageNode(
      name,
      contentForClass(c.asInstanceOf[DClass]),
      JSet(c.getDri),
      c.asInstanceOf[DClass],
      JNil,
      JNil,
    ).modified(name, pagesForMembers(c)) // We need override default page
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
        List("" -> m.getPackages.asScala.toList.filter(_.allMembers.nonEmpty)),
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

  override def contentForClasslike(c: DClasslike) = throw UnsupportedOperationException(
      s"Unable to generate DClasslike using default dokka method for $c!")

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


  extension (b: DocBuilder)
    def descriptionIfNotEmpty(d: Documentable): DocBuilder = {
      val desc = this.contentForDescription(d).asScala.toSeq
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

    def contentForDescription(m: Member) = b.memberInfo(m)

    def contentForScope(s: Documentable & WithScope & WithExtraProperties[_]) =
      def groupExtensions(extensions: Seq[Member]): Seq[DocumentableSubGroup] =
        extensions.groupBy(_.kind).map {
          case (Kind.Extension(on, _), members) =>
            val signature = Signature(s"extension (${on.name}: ") join on.signature join Signature(")")
            DocumentableSubGroup(signature, members.toSeq)
          case other => sys.error(s"unexpected value: $other")
        }.toSeq

      val (definedMethods, inheritedMethods) = s.membersBy(_.kind.isInstanceOf[Kind.Def]).byInheritance
      val (definedFields, inheritedFiles) = s.membersBy(m => m.kind == Kind.Val || m.kind == Kind.Var).byInheritance
      val (definedClasslikes, inheritedClasslikes) = s.membersBy(m => m.kind.isInstanceOf[Classlike]).byInheritance
      val (definedTypes, inheritedTypes) = s.membersBy(_.kind.isInstanceOf[Kind.Type]).byInheritance
      val (definedGivens, inheritedGives) = s.membersBy(_.kind.isInstanceOf[Kind.Given]).byInheritance
      val (definedExtensions, inheritedExtensions) = s.membersBy(_.kind.isInstanceOf[Kind.Extension]).byInheritance
      val (definedExports, inheritedExports) = s.membersBy(_.kind.isInstanceOf[Kind.Exported]).byInheritance
      val (definedImplicits, inheritedImplicits) = s.membersBy(_.kind.isInstanceOf[Kind.Implicit]).byInheritance

      b
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
        .documentableTab("Exports")(
          DocumentableGroup(Some("Defined exports"), definedExports),
          DocumentableGroup(Some("Inherited exports"), inheritedExports),
        )


    def contentForEnum(c: DClass) =
      b.documentableTab("Enum entries")(
        DocumentableGroup(None, c.membersBy(_.kind == Kind.EnumCase)) // Enum entries cannot be inherited
      )


    def contentForConstructors(c: DClass) =
       b.documentableTab("Constructors")(
        DocumentableGroup(None, c.membersBy(_.kind.isInstanceOf[Kind.Constructor]))
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
