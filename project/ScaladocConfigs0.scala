package dotty.tools.sbtplugin

import java.io.File
import java.nio.file.Path

import sbt._

object ScaladocConfigs0 {

  private lazy val currentYear: String = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR).toString

  val dottyExternalMapping = ".*scala/.*::scaladoc3::https://nightly.scala-lang.org/api/"
  val javaExternalMapping = ".*java/.*::javadoc::https://docs.oracle.com/en/java/javase/17/docs/api/java.base/"
  def defaultSourceLinks(version: String, allowGitSHA: Boolean = true): String = {
    def dottySrcLink(v: String) = sys.env.get("GITHUB_SHA") match {
      case Some(sha) if allowGitSHA => s"github://scala/scala3/$sha"
      case None => s"github://scala/scala3/$v"
    }
    s"-source-links:${dottySrcLink(version)},docs=github://scala/scala3/main#docs"
  }

  def DefaultGenerationSettings(projectVersion: String) = {
    val socialLinks = "-social-links:" + List(
      "github::https://github.com/scala/scala3",
      "discord::https://discord.com/invite/scala",
      "twitter::https://twitter.com/scala_lang",
    ).mkString(",")
    val skipById = "-skip-by-id:" + List(
      "scala.runtime.stdLibPatches",
      "scala.runtime.MatchCase",
    ).mkString(",")
    Array(
      "-project-version", projectVersion,
      "-Ygenerate-inkuire:true",
      defaultSourceLinks(version = Versions.dottyVersion),
      "-skip-by-regex", List(".+\\.internal($|\\..+)", ".+\\.impl($|\\..+)").mkString(","),
      skipById,
      "-project-logo", "docs/_assets/images/logo.svg",
      socialLinks,
      "-project-footer", s"Copyright (c) 2002-$currentYear, LAMP/EPFL",
      "-default-template", "static-site-main",
      "-author:true",
      "-groups:true",
      "-quick-links", List(
        "Learn::https://docs.scala-lang.org/",
        "Install::https://www.scala-lang.org/download/",
        "Playground::https://scastie.scala-lang.org",
        "Find\u00A0A\u00A0Library::https://index.scala-lang.org",
        "Community::https://www.scala-lang.org/community/",
        "Blog::https://www.scala-lang.org/blog/",
      ).mkString(",")
    )
  }

  def Scaladoc(projectVersion: String, classDir: File) = {
    val outDir = "scaladoc/output/self"
    IO.createDirectory(file(outDir))
    DefaultGenerationSettings(projectVersion) ++ Array(
      "-usejavacp:true",
      "-project", "scaladoc",
      "-d", outDir,
      "-revision", VersionUtil.gitHash,
      "-external-mappings", List(dottyExternalMapping, javaExternalMapping).mkString(","),
      // targets:
      classDir.getAbsolutePath
    )
  }

  def Testcases(projectVersion: String, tastyRoots: Seq[String]) = {
    val outDir = "scaladoc/output/testcases"
    IO.createDirectory(file(outDir))
    DefaultGenerationSettings(projectVersion) ++ Array(
      "-usejavacp:true",
      "-d", outDir,
      "-project", "scaladoc testcases",
      "-revision", "main",
      "-snippet-compiler", "scaladoc-testcases/docs=compile",
      "-siteroot", "scaladoc-testcases/docs",
      "-comment-syntax", List(
          "scaladoc-testcases/src/example/comment-md=markdown",
          "scaladoc-testcases/src/example/comment-wiki=wiki"
        ).mkString(","),
      "-external-mappings", List(dottyExternalMapping, javaExternalMapping).mkString(","),
    // targets:
    ) ++ tastyRoots
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
    s"$docsRoot/_docs/reference/experimental/capture-checking/classifiers.md=compile+test",
    s"$docsRoot/_docs/reference/experimental/capture-checking/scoped-capabilities.md=compile+test"
  )
  def referenceSnippetCompilerTargets(docsRoot: String) =
    referenceSnippetRelativeRoots.map(path => s"$docsRoot/_docs/reference/$path=compile") ++
      captureCheckingSnippetTestTargets(docsRoot)

  val scala3Output = "scaladoc/output/scala3"
  def Scala3(projectVersion: String, projectRoot: Path, stdlibRoot: Path, libraryProducts: Seq[String]): Array[String] = {
    // relative path to the stdlib directory ('library/')
    val stdlib = projectRoot.relativize(stdlibRoot.normalize())

    IO.createDirectory(file(scala3Output))
    DefaultGenerationSettings(projectVersion) ++ Array(
      "-project", "Scala 3",
      "-d", scala3Output,
      "-revision", "main",
      "-external-mappings", javaExternalMapping,
      "-doc-root-content", (stdlib / "resources" / "rootdoc.txt").toString,
      "-comment-syntax", "markdown", // Markdown syntax is used by default for all sources
      "-versions-dictionary-url", "https://scala-lang.org/api/versions.json",
      "-Ydocument-synthetic-types:true",
      "-snippet-compiler", (snippetCompilerTargets(s"$stdlib/src") ++ referenceSnippetCompilerTargets("docs")).mkString(","),
      "-no-snippet-names-for", "_docs/reference",
      "-siteroot", "docs",
      "-Yapi-subdirectory:true",
    // targets:
    ) ++ libraryProducts
  }

  val referenceDocumentationOutputDir = "scaladoc/output/reference"
  def Reference(docs: File, baseConfig: Array[String]): Array[String] = {
    IO.createDirectory(file(referenceDocumentationOutputDir))
    baseConfig ++ Array(
      "-d", referenceDocumentationOutputDir,
      "-siteroot", docs.getAbsolutePath,
      "-project", "Scala 3 Reference",
      "-project-version", Versions.baseVersion,
      "-source-links", s"${docs.getAbsolutePath}=github://scala/scala3/language-reference-stable#docs",
      "-Ygenerate-api:false",
      "-snippet-compiler", ScaladocConfigs0.referenceSnippetCompilerTargets(docs.getAbsolutePath).mkString(","),
      "-versions-dictionary-url", "", // override it
    )
  }

  def ReferenceSnippets(baseConfig: Array[String], outputDir: String, tempRoot: String): Array[String] = {
    baseConfig ++ Array(
      "-d", outputDir,
      "-siteroot", tempRoot,
      "-project", "Scala 3 Reference Snippets",
      "-project-version", Versions.baseVersion,
      "-source-links", s"$tempRoot=github://scala/scala3/main#docs",
      "-no-link-warnings:true",
      "-no-link-asset-warnings:true",
      "-Ygenerate-api:false",
      "-snippet-compiler", ScaladocConfigs0.referenceSnippetCompilerTargets(tempRoot).mkString(","),
      "-versions-dictionary-url", "", // override it
    )
  }

  def StableDoc(baseConfig: Array[String], version: String, siteRoot: String): Array[String] = {
    baseConfig ++ Array(
      "-project-version", version,
      "-revision", version,
      "-d", s"scaladoc/output/$version",
      "-siteroot", siteRoot,
      "-Yapi-subdirectory:false",
      ScaladocConfigs0.defaultSourceLinks(version = version, allowGitSHA = false),
    )
  }
}
