package dotty.dokka
package site

import java.nio.file.Files
import java.nio.file.FileVisitOption

import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages._

import scala.collection.JavaConverters._
import dotty.dokka.model.api._

class SiteResourceManager(using ctx: DocContext) extends BaseStaticSiteProcessor:
    private def listResources(nodes: Seq[PageNode]): Set[String] =
      nodes.flatMap {
        case it: AContentPage =>
          listResources(it.getChildren.asScala.toList)
        case it: StaticPageNode =>
          listResources(it.getChildren.asScala.toList) ++ it.resources()
        case _ => Seq.empty
      }.toSet

    override def transform(input: RootPageNode, ctx: StaticSiteContext): RootPageNode =
      val rootPath = ctx.root.toPath
      val imgPath = rootPath.resolve("images")
      val images =
        if !Files.exists(imgPath) then Nil
        else
          val allPaths = Files.walk(imgPath, FileVisitOption.FOLLOW_LINKS)
          val files = allPaths.filter(Files.isRegularFile(_)).iterator().asScala
          files.map(p => rootPath.relativize(p).toString).toList

      val resources = images ++ listResources(input.getChildren.asScala.toList)

      val resourcePages = resources.map { path =>
        val strategy = new RenderingStrategy.Copy(rootPath.resolve(path).toString)
        new RendererSpecificResourcePage(path, JList(), strategy)
      }

      val modified = input.transformContentPagesTree {
        case it: StaticPageNode =>
          it.copy(getEmbeddedResources =
            if it.template.hasFrame then it.getEmbeddedResources ++ it.resources().asJava
            else it.resources().asJava
          )
        case it => it
      }
      val newChildren = (resourcePages ++ modified.getChildren.asScala).asJava
      modified.modified(modified.getName, newChildren)