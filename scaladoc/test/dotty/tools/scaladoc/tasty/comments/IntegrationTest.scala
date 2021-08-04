package dotty.tools.scaladoc
package tasty.comments

import org.junit.Test

abstract class BaseIntegrationTest(pck: String) extends BaseHtmlTest:

  @Test
  def testLinks: Unit =  withGeneratedDoc(pcks = Seq(pck, "commonlinks")) {
    def checkDocLinks(links: String*)(ctx: DocumentContext): Unit =
      ctx.assertAttr(".documentableBrief a, .cover a", "href", links:_*)
      ctx.assertNotExists("unresolvedLinkSelector")

    def checkUnresolved(ctx: DocumentContext): Unit =
      ctx.assertAttr(
        unresolvedLinkSelector,
        "data-unresolved-link",
        "", "" // each represent a link
        )

    withHtmlFile(s"tests/$pck/BrokenLinks.html")(checkUnresolved)
    val otherPackagePath = "../commonlinks/SomeOtherPackage.html"
    withHtmlFile(s"tests/$pck/OtherPackageLink.html")(checkDocLinks(otherPackagePath))
    // OtherPackageMembers - does not work, TODO?
    withHtmlFile(s"tests/$pck/SamePackageLink.html")(checkDocLinks("SomeClass.html"))
    // SamePackageMembers - does not work, TODO?
  }


class MarkdownIntegrationTest extends BaseIntegrationTest("mdlinks")

class WikiIntegrationTest extends BaseIntegrationTest("wikilinks")
