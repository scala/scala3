package dotty.tools.sbtplugin

import java.io.File
import java.nio.file.Path

import sbt._

object ScaladocConfigs0 {
  import ScaladocGeneration._

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  def dottyExternalMapping = ".*scala/.*::scaladoc3::https://nightly.scala-lang.org/api/"
  def javaExternalMapping = ".*java/.*::javadoc::https://docs.oracle.com/en/java/javase/17/docs/api/java.base/"
  def defaultSourceLinks(version: String, allowGitSHA: Boolean = true) = {
    def dottySrcLink(v: String) = sys.env.get("GITHUB_SHA") match {
      case Some(sha) if allowGitSHA => s"github://scala/scala3/$sha"
      case None => s"github://scala/scala3/$v"
    }
    SourceLinks(List(dottySrcLink(version), "docs=github://scala/scala3/main#docs"))
  }

  def DefaultGenerationSettings(projectVersion: String) = {
    def socialLinks = SocialLinks(List(
      "github::https://github.com/scala/scala3",
      "discord::https://discord.com/invite/scala",
      "twitter::https://twitter.com/scala_lang",
    ))
    def projectLogo = ProjectLogo("docs/_assets/images/logo.svg")
    def skipByRegex = SkipByRegex(List(".+\\.internal($|\\..+)", ".+\\.impl($|\\..+)"))
    def skipById = SkipById(List(
      "scala.runtime.stdLibPatches",
      "scala.runtime.MatchCase",
    ))
    def projectFooter = ProjectFooter(s"Copyright (c) 2002-$currentYear, LAMP/EPFL")
    def defaultTemplate = DefaultTemplate("static-site-main")
    GenerationConfig(
      List(),
      ProjectVersion(projectVersion),
      GenerateInkuire(true),
      defaultSourceLinks(version = Versions.dottyVersion),
      skipByRegex,
      skipById,
      projectLogo,
      socialLinks,
      projectFooter,
      defaultTemplate,
      Author(true),
      Groups(true),
      QuickLinks(
        List(
          "Learn::https://docs.scala-lang.org/",
          "Install::https://www.scala-lang.org/download/",
          "Playground::https://scastie.scala-lang.org",
          "Find\u00A0A\u00A0Library::https://index.scala-lang.org",
          "Community::https://www.scala-lang.org/community/",
          "Blog::https://www.scala-lang.org/blog/",
        )
      )
    )
  }

  def Scaladoc(projectVersion: String, classDir: File) = {
    DefaultGenerationSettings(projectVersion)
      .add(UseJavacp(true))
      .add(ProjectName("scaladoc"))
      .add(OutputDir("scaladoc/output/self"))
      .add(Revision(VersionUtil.gitHash))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets(classDir.getAbsolutePath :: Nil)
  }

  def Testcases(projectVersion: String, tastyRoots: Seq[String]) = {
    DefaultGenerationSettings(projectVersion)
      .add(UseJavacp(true))
      .add(OutputDir("scaladoc/output/testcases"))
      .add(ProjectName("scaladoc testcases"))
      .add(Revision("main"))
      .add(SnippetCompiler(List("scaladoc-testcases/docs=compile")))
      .add(SiteRoot("scaladoc-testcases/docs"))
      .add(CommentSyntax(List(
        "scaladoc-testcases/src/example/comment-md=markdown",
        "scaladoc-testcases/src/example/comment-wiki=wiki"
      )))
      .add(ExternalMappings(List(dottyExternalMapping, javaExternalMapping)))
      .withTargets(tastyRoots)
  }

  def snippetCompilerTargets(dottyLibSrc:String) = List(
    s"$dottyLibSrc/scala=compile"
  )
  // Relative subtrees in `_docs/reference` where snippet compilation is explicitly enabled.
  // Keep this shared with the full docs tasks and the lightweight snippet-check task.
  def referenceSnippetRelativeRoots = List(
    "new-types",
    "enums",
    "experimental/capture-checking",
  )
  def captureCheckingSnippetTestTargets(docsRoot: String) = List(
    s"$docsRoot/_docs/reference/experimental/capture-checking/basics.md=compile+test",
    s"$docsRoot/_docs/reference/experimental/capture-checking/checked-exceptions.md=compile+test",
    s"$docsRoot/_docs/reference/experimental/capture-checking/scoped-capabilities.md=compile+test"
  )
  def referenceSnippetCompilerTargets(docsRoot: String) =
    referenceSnippetRelativeRoots.map(path => s"$docsRoot/_docs/reference/$path=compile") ++
      captureCheckingSnippetTestTargets(docsRoot)

  def Scala3(projectVersion: String, projectRoot: Path, stdlibRoot: Path, libraryProducts: Seq[String]) = {
    // relative path to the stdlib directory ('library/')
    val stdlib = projectRoot.relativize(stdlibRoot.normalize())

    DefaultGenerationSettings(projectVersion)
      .add(ProjectName("Scala 3"))
      .add(OutputDir(file("scaladoc/output/scala3").getAbsoluteFile.getAbsolutePath))
      .add(Revision("main"))
      .add(ExternalMappings(List(javaExternalMapping)))
      .add(DocRootContent((stdlib / "resources" / "rootdoc.txt").toString))
      .add(CommentSyntax(List(
        // Markdown syntax is used by default for all sources
        "markdown"
      )))
      .add(VersionsDictionaryUrl("https://scala-lang.org/api/versions.json"))
      .add(DocumentSyntheticTypes(true))
      .add(SnippetCompiler(
        snippetCompilerTargets(s"$stdlib/src") ++ referenceSnippetCompilerTargets("docs")
      ))
      .add(NoSnippetNamesFor(List("_docs/reference")))
      .add(SiteRoot("docs"))
      .add(ApiSubdirectory(true))
      .withTargets(libraryProducts)
  }

}
