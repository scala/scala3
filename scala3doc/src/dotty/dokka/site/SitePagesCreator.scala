package dotty.dokka
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.FileVisitOption

import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages._

import scala.collection.JavaConverters._

import dotty.dokka.model.api._

class SitePagesCreator(using ctx: DocContext) extends BaseStaticSiteProcessor:
  private def mkRootPage(input: RootPageNode, children: List[PageNode]): AContentPage =
    input match
      case input: ContentPage =>
        AContentPage(
          input.getName,
          children.asJava,
          input.getContent,
          JSet(apiPageDRI),
          input.getEmbeddedResources
        )
      case _: RendererSpecificRootPage =>
        children.filter(_.isInstanceOf[RootPageNode]) match
            case List(nestedRoot: RootPageNode) =>
              mkRootPage(nestedRoot, children.filter { _ != nestedRoot } ++
                nestedRoot.getChildren.asScala)
            case other =>
              throw new RuntimeException(s"Expected single nested roor but get: $other")

      case _ => throw new RuntimeException(s"UNSUPPORTED! ${input.getClass.getName}")


  override def transform(input: RootPageNode, ctx: StaticSiteContext): RootPageNode =
    val (contentPage, others) =
      input.getChildren.asScala.toList.partition { _.isInstanceOf[ContentPage] }

    val apiRoot = mkRootPage(input, contentPage)
    val (indexes, children) = ctx.allPages.partition(f =>
      f.template.isIndexPage() && f.template.file.toPath.getParent() == ctx.docsPath )

    if (indexes.size > 1)
      val msg = s"ERROR: Multiple index pages for doc found ${indexes.map(_.template.file)}"
      report.error(msg)

    def emptyContent = ctx.asContent(Text(), DRI(extra = "root_content")).get(0)

    val root = ctx.indexPage().toList.map(_.copy(getDri = JSet(docsRootDRI)))
    val docsRoot = AContentPage(
      ctx.args.name,
      (List(apiRoot.modified("API", apiRoot.getChildren)) ++ children ++ root).asJava,
      indexes.headOption.fold(emptyContent)(_.getContent),
      JSet(docsDRI),
      JList()
    )

    new RendererSpecificRootPage(
      apiRoot.getName,
      (List(docsRoot) ++ others).asJava,
      RenderingStrategy.DoNothing.INSTANCE
    )
