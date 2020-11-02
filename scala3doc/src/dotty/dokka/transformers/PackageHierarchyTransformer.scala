package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext

class PackageHierarchyTransformer(context: DokkaContext) extends PageTransformer:
  override def invoke(input: RootPageNode): RootPageNode = input match {
    case m: ModulePageNode => rearangePackagePages(m)
    case other => {
      context.getLogger.warn("PackageHierarchyTransformer: Couldn't transform root because root is not ModulePageNode")
      other
    }
  }

  def rearangePackagePages(page: ModulePageNode): ModulePageNode = {
    val (h1, h2) = page.getChildren.asScala.partition{
      case p: PackagePageNode => true
      case other => false
    }

    val (packagePages, otherPages) = (h1.collect{ case p: PackagePageNode => p}.toSeq, h2.collect{ case q: PageNode => q }.toSeq )

    def isParent(possibleParent: Seq[String], comparedChildren: Seq[String]): Boolean = comparedChildren.startsWith(possibleParent)

    def getRelativeName(parent: Seq[String], child: Seq[String]): Seq[String] = child.slice(parent.size, child.size)

    def relativePageName(parentName: Seq[String], childName: Seq[String], childPage: PageNode): PageNode =
      childPage.modified(
        childName.slice(parentName.size, childName.size).mkString("",".",""),
        childPage.getChildren
      )

    def buildPackageTree(
      depth: Int,
      remaining: Seq[(Seq[String], PackagePageNode)],
      processsed: Seq[(Seq[String], PackagePageNode)]
    ): Seq[PackagePageNode] = {
      val (currentDepth,rest) = remaining.partition((tokens, page) => tokens.size == depth)
      val newProcessed = currentDepth.map( (tokens, page) => {
        val newPage = page.modified(
          page.getName,
          (processsed
            .filter((childTokens, child) => isParent(tokens, childTokens))
            .map((childTokens, child) => relativePageName(tokens, childTokens, child))
          ++ page.getChildren.asScala).asJava,
        )
        (tokens, newPage)
        }
      )
      val oldFilteredProcessed = processsed
        .filter( (tokens, page) =>
          currentDepth.forall( (parentTokens, parentPage) =>
            !isParent(parentTokens, tokens)
          )
        )

      if(depth == 1) (newProcessed ++ oldFilteredProcessed).map(_(1))
      else buildPackageTree(depth - 1, rest, newProcessed ++ oldFilteredProcessed)
    }

    val packagePagesWithTokens = packagePages.map(page => (("""\.""".r.split(page.getName)).toSeq, page))

    val maxDepthElem = packagePagesWithTokens.maxBy( (tokens, page) => tokens.size )

    page.modified(
      page.getName,
      (otherPages ++ buildPackageTree(maxDepthElem(0).size, packagePagesWithTokens, Seq.empty)).asJava
    )


  }
