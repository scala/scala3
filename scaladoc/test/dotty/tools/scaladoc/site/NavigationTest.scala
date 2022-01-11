package dotty.tools.scaladoc
package site

import org.junit.Test

class NavigationTest extends BaseHtmlTest:

  case class NavMenuTestEntry( name: String, link: String, nested: Seq[NavMenuTestEntry])

  def testNavMenu(page: String, topLevel: NavMenuTestEntry)(using ProjectContext): Unit =
    withHtmlFile(page){ content  =>

      def test(query: String, el: Seq[NavMenuTestEntry]) =
        content.assertTextsIn(query, el.map(_.name):_*)
        content.assertAttr(query,"href", el.map(_.link):_*)

      test("#sideMenu2>div>span>a", topLevel :: Nil)
      test("#sideMenu2>div>div>span>a", topLevel.nested)
      test("#sideMenu2>div>div>div>span>a", topLevel.nested.flatMap(_.nested))
      test("#sideMenu2>div>div>div>div>span>a", topLevel.nested.flatMap(_.nested.flatMap(_.nested)))
    }


  @Test
  def testBasicNavigation() = withGeneratedSite(testDocPath.resolve("basic")) {
    val topLevelNav = NavMenuTestEntry(projectName, "index.html", Seq(
      NavMenuTestEntry("A directory", "dir/index.html", Seq(
        NavMenuTestEntry("Nested in a directory", "dir/nested.html", Nil)
      )),
      NavMenuTestEntry("Adoc", "Adoc.html", Seq()),
      NavMenuTestEntry("API", "../index.html", Seq(
        NavMenuTestEntry("tests.site", "../tests/site.html", Seq(
          NavMenuTestEntry("BrokenLink", "../tests/site/BrokenLink.html", Nil),
          NavMenuTestEntry("BrokenLinkWiki", "../tests/site/BrokenLinkWiki.html", Nil),
          NavMenuTestEntry("OtherPackageLink", "../tests/site/OtherPackageLink.html", Nil),
          NavMenuTestEntry("OtherPackageLinkWiki", "../tests/site/OtherPackageLinkWiki.html", Nil),
          NavMenuTestEntry("SamePackageLink", "../tests/site/SamePackageLink.html", Nil),
          NavMenuTestEntry("SamePackageLinkWiki", "../tests/site/SamePackageLinkWiki.html", Nil),
          NavMenuTestEntry("SomeClass", "../tests/site/SomeClass.html", Nil)
        )),
        NavMenuTestEntry("tests.site.some.other", "../tests/site/some/other.html", Seq(
          NavMenuTestEntry("SomeOtherPackage", "../tests/site/some/other/SomeOtherPackage.html", Nil),
        ))
      )),
    ))

    testNavMenu("docs/Adoc.html", topLevelNav)
  }
