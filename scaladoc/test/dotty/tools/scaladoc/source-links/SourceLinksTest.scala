package dotty.tools.scaladoc
package sourcelinks

import java.nio.file._
import org.junit.Assert._
import org.junit.Test

class SourceLinkTest:

  @Test
  def testBasicFailures() =
    def testFailure(template: String, messagePart: String) =
      val res = SourceLinkParser(None).parse(template)
      assertTrue(s"Expected failure containing $messagePart: $res", res.left.exists(_.contains(messagePart)))

      val resWithVersion = SourceLinkParser(Some("develop")).parse(template)
      assertEquals(res, resWithVersion)

    testFailure("ala://ma/kota", "known provider")
    testFailure("ala=ala=ala://ma/kota", "source link syntax")
    testFailure("ala=ala=ala", "source link syntax")
    testFailure("""€{TPL_OWNER}""", "scaladoc")


  @Test
  def testProperTemplates() =
    def test(template: String) =
      val res = try SourceLinkParser(Some("develop")).parse(template) catch
        case e: Exception => throw RuntimeException(s"When testing $template", e)
      assertTrue(s"Bad template: $template", res.isRight)


    Seq(
      "github://lampepfl/dotty",
      "gitlab://lampepfl/dotty",
      "github://lampepfl/dotty/branch/withslash",
      "https://github.com/scala/scala/blob/2.13.x€{FILE_PATH_EXT}#€{FILE_LINE}"
    ).foreach{ template =>
      test(template)
    }


  @Test
  def testSourceProviderWithoutRevision() =
    Seq("github", "gitlab").foreach { provider =>
      val template = s"$provider://ala/ma"
      val res = SourceLinkParser(None).parse(template)
      assertTrue(s"Expected failure containing missing revision: $res", res.left.exists(_.contains("revision")))
    }

class SourceLinksTest:
  // TODO (https://github.com/lampepfl/scaladoc/issues/240): configure source root
  val projectRoot = Paths.get("").toAbsolutePath()

  val edit: Operation = "edit" // union types need explicit singletons

  type Args = String | (String, Operation) | (String, Int) | (String, Int, Operation)

  private def testLink(config: Seq[String], revision: Option[String])(cases: (Args, String | None.type)*): Unit =
    val links = SourceLinks.load(config, revision)(using testContext)
    cases.foreach { case (args, expected) =>
      val res = args match
        case path: String => links.pathTo(projectRoot.resolve(path))
        case (path: String, line: Int) => links.pathTo(projectRoot.resolve(path), line = Some(line))
        case (path: String, operation: Operation) => links.pathTo(projectRoot.resolve(path), operation = operation)
        case (path: String, line: Int, operation: Operation) => links.pathTo(projectRoot.resolve(path), operation = operation, line = Some(line))

      val expectedRes = expected match
        case s: String => Some(s)
        case None => None

      assertEquals(s"For path $args", expectedRes, res)
    }

  @Test
  def testBasicPaths =
    testLink(Seq("github://lampepfl/dotty"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/develop/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/develop/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/develop/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/develop/project/Build.scala#L54",
    )

    testLink(Seq("github://lampepfl/dotty/dev"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/dev/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/dev/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/dev/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/dev/project/Build.scala#L54",
    )

    testLink(Seq("github://lampepfl/dotty/dev#src/lib"), None)(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/dev/src/lib/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/dev/src/lib/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/dev/src/lib/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/dev/src/lib/project/Build.scala#L54",
    )

    testLink(Seq("github://lampepfl/dotty/dev#src/lib"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/dev/src/lib/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/dev/src/lib/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/dev/src/lib/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/dev/src/lib/project/Build.scala#L54",
    )

    testLink(Seq("github://lampepfl/dotty#src/lib"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/develop/src/lib/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/develop/src/lib/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/develop/src/lib/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/develop/src/lib/project/Build.scala#L54",
    )

    testLink(Seq("gitlab://lampepfl/dotty"), Some("develop"))(
      "project/Build.scala" -> "https://gitlab.com/lampepfl/dotty/-/blob/develop/project/Build.scala",
      ("project/Build.scala", 54) -> "https://gitlab.com/lampepfl/dotty/-/blob/develop/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/project/Build.scala#L54",
    )

    testLink(Seq("€{FILE_PATH}.scala#€{FILE_LINE}"), Some("develop"))(
      "project/Build.scala" -> "/project/Build.scala#",
      ("project/Build.scala", 54) -> "/project/Build.scala#54",
      ("project/Build.scala", edit) -> "/project/Build.scala#",
      ("project/Build.scala", 54, edit) -> "/project/Build.scala#54",
    )

    testLink(Seq("https://github.com/scala/scala/blob/2.13.x€{FILE_PATH_EXT}#L€{FILE_LINE}"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L",
      ("project/Build.scala", 54) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L",
      ("project/Build.scala", 54, edit) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L54",
    )

    testLink(Seq("https://github.com/scala/scala/blob/2.13.x€{FILE_PATH}.scala#L€{FILE_LINE}"), Some("develop"))(
      "project/Build.scala" -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L",
      ("project/Build.scala", 54) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L",
      ("project/Build.scala", 54, edit) -> "https://github.com/scala/scala/blob/2.13.x/project/Build.scala#L54",
    )

    testLink(Seq("github://lampepfl/dotty/branch/withslash#src/lib"), None)(
      "project/Build.scala" -> "https://github.com/lampepfl/dotty/blob/branch/withslash/src/lib/project/Build.scala",
      ("project/Build.scala", 54) -> "https://github.com/lampepfl/dotty/blob/branch/withslash/src/lib/project/Build.scala#L54",
      ("project/Build.scala", edit) -> "https://github.com/lampepfl/dotty/edit/branch/withslash/src/lib/project/Build.scala",
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/branch/withslash/src/lib/project/Build.scala#L54",
    )

  @Test
  def testBasicPrefixedPaths =
    testLink(Seq("src=gitlab://lampepfl/dotty"), Some("develop"))(
      "src/lib/core.scala" -> "https://gitlab.com/lampepfl/dotty/-/blob/develop/lib/core.scala",
      ("src/lib/core.scala", 33, edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/lib/core.scala#L33",
      ("src/lib/core.scala", 33, edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/lib/core.scala#L33",
      "build.sbt" -> None
    )


  @Test
  def prefixedPaths =
    testLink(Seq(
     "src/generated=€{FILE_PATH_EXT}#€{FILE_LINE}",
      "src=gitlab://lampepfl/dotty",
      "github://lampepfl/dotty"
      ), Some("develop"))(
      ("project/Build.scala", 54, edit) -> "https://github.com/lampepfl/dotty/edit/develop/project/Build.scala#L54",
      ("src/lib/core.scala", 33, edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/lib/core.scala#L33",
      ("src/generated.scala", 33, edit) -> "https://gitlab.com/lampepfl/dotty/-/edit/develop/generated.scala#L33",
      ("src/generated/template.scala", 1, edit) -> "/template.scala#1"
    )
