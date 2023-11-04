package dotty.tools.scaladoc
package site

import org.junit.Test

class NavigationTest extends BaseHtmlTest:

  case class NavMenuTestEntry( name: String, link: String, nested: Seq[NavMenuTestEntry])

  def testNavMenu(page: String, topLevel: Seq[NavMenuTestEntry])(using ProjectContext): Unit =
    withHtmlFile(page){ content  =>

      def test(query: String, el: Seq[NavMenuTestEntry]) =
        content.assertTextsIn(query, el.map(_.name)*)
        content.assertAttr(query,"href", el.map(_.link)*)

      test(".side-menu>div>span>a", topLevel)
      test(".side-menu>div>div>span>a", topLevel.flatMap(_.nested))
    }


  @Test
  def testBasicNavigation() = withGeneratedSite(testDocPath.resolve("basic")) {
    val docsNav = Seq(
      NavMenuTestEntry("A directory", "dir/index.html", Seq(
        NavMenuTestEntry("Nested in a directory", "dir/nested.html", Nil)
      )),
      NavMenuTestEntry("Adoc", "#", Seq())
    )

    val apiNav = Seq(
      NavMenuTestEntry("tests.site", "#", Seq(
        NavMenuTestEntry("some.other", "site/some/other.html", Seq(
          NavMenuTestEntry("SomeOtherPackage", "site/some/other/SomeOtherPackage.html", Nil),
        )),
        NavMenuTestEntry("BrokenLink", "site/BrokenLink.html", Nil),
        NavMenuTestEntry("BrokenLinkWiki", "site/BrokenLinkWiki.html", Nil),
        NavMenuTestEntry("OtherPackageLink", "site/OtherPackageLink.html", Nil),
        NavMenuTestEntry("OtherPackageLinkWiki", "site/OtherPackageLinkWiki.html", Nil),
        NavMenuTestEntry("SamePackageLink", "site/SamePackageLink.html", Nil),
        NavMenuTestEntry("SamePackageLinkWiki", "site/SamePackageLinkWiki.html", Nil),
        NavMenuTestEntry("SomeClass", "site/SomeClass.html", Nil)
      )),
    )

    testNavMenu("docs/Adoc.html", docsNav)
    testNavMenu("tests/site.html", apiNav)
  }
