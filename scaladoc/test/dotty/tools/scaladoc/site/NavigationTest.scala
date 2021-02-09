package dotty.tools.scaladoc
package site

import org.junit.Test

class NavigationTest extends BaseHtmlTest:

  case class NavMenuTestEntry( name: String, link: String, nested: Seq[NavMenuTestEntry])

  def testNavMenu(page: String, topLevel: NavMenuTestEntry)(using ProjectContext): Unit =
    withHtmlFile(page){ content  =>
      def flatten(l: NavMenuTestEntry): Seq[NavMenuTestEntry] = l +: l.nested.flatMap(flatten)

      def test(query: String, el: Seq[NavMenuTestEntry]) =
        content.assertTextsIn(query, el.map(_.name):_*)
        content.assertAttr(query,"href", el.map(_.link):_*)

      test("#sideMenu2 a", flatten(topLevel))
      test("#sideMenu2>div>div>a", topLevel.nested)
      test("#sideMenu2>div>div>div>a", topLevel.nested.flatMap(_.nested))
      test("#sideMenu2>div>div>div>div>a", topLevel.nested.flatMap(_.nested.flatMap(_.nested)))
    }


  @Test
  def testBasicNavigation() = withGeneratedSite(testDocPath.resolve("basic")){
    val topLevelNav = NavMenuTestEntry(projectName, "index.html", Seq(
      NavMenuTestEntry("A directory", "dir/index.html", Seq(
        NavMenuTestEntry("Nested in a directory", "dir/nested.html", Nil)
      )),
      NavMenuTestEntry("Adoc", "Adoc.html", Seq()),
      NavMenuTestEntry("Basic test", "../index.html", Seq()),
      NavMenuTestEntry("API", "../api/index.html", Seq(
        NavMenuTestEntry("tests.site", "../api/tests/site.html", Seq(
          NavMenuTestEntry("BrokenLink", "../api/tests/site/BrokenLink.html", Nil),
          NavMenuTestEntry("BrokenLinkWiki", "../api/tests/site/BrokenLinkWiki.html", Nil),
          NavMenuTestEntry("OtherPackageLink", "../api/tests/site/OtherPackageLink.html", Nil),
          NavMenuTestEntry("OtherPackageLinkWiki", "../api/tests/site/OtherPackageLinkWiki.html", Nil),
          NavMenuTestEntry("SamePackageLink", "../api/tests/site/SamePackageLink.html", Nil),
          NavMenuTestEntry("SamePackageLinkWiki", "../api/tests/site/SamePackageLinkWiki.html", Nil),
          NavMenuTestEntry("SomeClass", "../api/tests/site/SomeClass.html", Nil)
        )),
        NavMenuTestEntry("tests.site.some.other", "../api/tests/site/some/other.html", Seq(
          NavMenuTestEntry("SomeOtherPackage", "../api/tests/site/some/other/SomeOtherPackage.html", Nil),
        ))
      )),
    ))

    testNavMenu("docs/Adoc.html", topLevelNav)
  }