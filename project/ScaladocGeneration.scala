package dotty.tools.sbtplugin

import java.nio.file.Files
import sbt._

object ScaladocGeneration {
  def generateCommand(config: GenerationConfig): String =
    " " + config.serialize

  sealed trait Arg[T] {
    def key: String
    def value: T

    def serialize: String =
      value match {
        case s: String => s"$key ${escape(s)}"
        case value: Boolean => s"$key:$value"
        case list: List[_] => s"$key:${list.map(x => escape(x.toString)).mkString(",")}"
        case _ =>
          println(s"Unsupported setting: $key -> $value")
          ""
      }

    private def escape(s: String) = if (s.contains(" ")) s""""$s"""" else s
  }
  case class ProjectName(value: String) extends Arg[String] {
    def key: String = "-project"
  }

  case class OutputDir(value: String) extends Arg[String] {
    def key: String = "-d"
  }

  case class ProjectVersion(value: String) extends Arg[String] {
    def key: String = "-project-version"
  }

  case class ProjectLogo(value: String) extends Arg[String] {
    def key: String = "-project-logo"
  }

  case class ProjectFooter(value: String) extends Arg[String] {
    def key: String = "-project-footer"
  }

  case class SourceLinks(value: List[String]) extends Arg[List[String]] {
    def key: String = "-source-links"
  }

  case class CommentSyntax(value: List[String]) extends Arg[List[String]] {
    def key: String = "-comment-syntax"
  }

  case class Revision(value: String) extends Arg[String] {
    def key: String = "-revision"
  }

  case class ExternalMappings(value: List[String]) extends Arg[List[String]] {
    def key: String = "-external-mappings"
  }

  case class SocialLinks(value: List[String]) extends Arg[List[String]] {
    def key: String = "-social-links"
  }

  case class SkipPackages(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-packages"
  }

  case class SkipById(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-by-id"
  }

  case class SkipByRegex(value: List[String]) extends Arg[List[String]] {
    def key: String = "-skip-by-regex"
  }

  case class DocRootContent(value: String) extends Arg[String] {
    def key: String = "-doc-root-content"
  }

  case class Author(value: Boolean) extends Arg[Boolean] {
    def key: String = "-author"
  }

  case class Groups(value: Boolean) extends Arg[Boolean] {
    def key: String = "-groups"
  }

  case class DocCanonicalBaseUrl(value: String) extends Arg[String] {
    def key: String = "-doc-canonical-base-url"
  }

  case class SiteRoot(value: String) extends Arg[String] {
    def key: String = "-siteroot"
  }

  case class NoLinkWarnings(value: Boolean) extends Arg[Boolean] {
    def key: String = "-no-link-warnings"
  }

  case class NoLinkAssetWarnings(value: Boolean) extends Arg[Boolean] {
    def key: String = "-no-link-asset-warnings"
  }

  case class VersionsDictionaryUrl(value: String) extends Arg[String] {
    def key: String = "-versions-dictionary-url"
  }

  case class DocumentSyntheticTypes(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Ydocument-synthetic-types"
  }

  case class SnippetCompiler(value: List[String]) extends Arg[List[String]] {
    def key: String = "-snippet-compiler"
  }

  case class GenerateInkuire(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Ygenerate-inkuire"
  }

  case class ApiSubdirectory(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Yapi-subdirectory"
  }

  case class ScastieConfiguration(value: String) extends Arg[String] {
    def key: String = "-scastie-configuration"
  }

  case class DefaultTemplate(value: String) extends Arg[String] {
    def key: String = "-default-template"
  }

  case class UseJavacp(value: Boolean) extends Arg[Boolean] {
    def key: String = "-usejavacp"
  }

  case class QuickLinks(value: List[String]) extends Arg[List[String]] {
    def key: String = "-quick-links"
  }

  case class DynamicSideMenu(value: Boolean) extends Arg[Boolean] {
    def key: String = "-dynamic-side-menu"
  }

  case class SuppressCC(value: Boolean) extends Arg[Boolean] {
    def key: String = "-suppressCC"
  }

  case class NoSnippetNamesFor(value: List[String]) extends Arg[List[String]] {
    def key: String = "-no-snippet-names-for"
  }

  case class GenerateAPI(value: Boolean) extends Arg[Boolean] {
    def key: String = "-Ygenerate-api"
  }

  import _root_.scala.reflect._

  trait GenerationConfig {
    def get[T <: Arg[_] : ClassTag]: Option[T]
    def add[T <: Arg[_]](arg: T): GenerationConfig
    def remove[T <: Arg[_]: ClassTag]: GenerationConfig
    def withTargets(targets: Seq[String]): GenerationConfig
    def serialize: String
    def settings: Seq[String]
  }

  object GenerationConfig {
    def apply(): GenerationConfig = GenerationConfigImpl(Seq.empty, Seq.empty)
    def apply(targets: List[String], args: Arg[_]*): GenerationConfig = args.foldLeft(GenerationConfig()) { (config, elem) =>
      config.add(elem)
    }
    private case class GenerationConfigImpl(targets: Seq[String], args: Seq[Arg[_]]) extends GenerationConfig {
      override def add[T <: Arg[_]](arg: T): GenerationConfig = {
        implicit val tag: ClassTag[T] = ClassTag(arg.getClass)
        val (removedElem, argsWithoutElem) = argsWithout[T](tag)
        removedElem.foreach(elem => println(s"$elem has been overwritten by $arg"))
        GenerationConfigImpl(targets, argsWithoutElem :+ arg)
      }
      override def remove[T <: Arg[_] : ClassTag]: GenerationConfig = {
        GenerationConfigImpl(targets, argsWithout[T]._2)
      }

      override def get[T <: Arg[_] : ClassTag]: Option[T] = {
        val tag = implicitly[ClassTag[T]]
        args.collect {
          case tag(t) => t
        }.headOption
      }

      override def withTargets(targets: Seq[String]) = copy(targets = targets)
      override def serialize: String = (
        args
        .map(_.serialize)
         ++ targets
      ).mkString(" ")

      override def settings: Seq[String] =
        args.map(_.serialize) ++ targets

      private def argsWithout[T <: Arg[_]](
        implicit tag: ClassTag[T]
      ): (Option[T], Seq[Arg[_]]) = args.foldLeft[(Option[T], Seq[Arg[_]])]((None, Seq.empty)) {
        case ((removedElem, rest), tag(t)) => (Some(t), rest)
        case ((removedElem, rest), elem) => (removedElem, rest :+ elem)
      }
    }
  }

  private val referenceDocumentationOutputDir = "scaladoc/output/reference"

  def prepareReferenceDocumentationDir(docs: File): Unit = {
    IO.copyDirectory(file("docs"), docs)
    IO.delete(docs / "_blog")

    // Add redirections from previously supported URLs, for some pages
    for (name <- Seq("changed-features", "contextual", "dropped-features", "metaprogramming", "other-new-features")) {
      val path = docs / "_docs" / "reference" / name / s"${name}.md"
      val contentLines = new collection.mutable.ArrayBuffer[String].++=(IO.read(path).linesIterator)
      contentLines.insert(1, s"redirectFrom: /${name}.html") // Add redirection
      val newContent = contentLines.mkString("\n")
      IO.write(path, newContent)
    }
  }

  def languageReferenceConfig(docs: File, baseConfig: GenerationConfig): GenerationConfig =
    baseConfig
      .add(OutputDir(referenceDocumentationOutputDir))
      .add(SiteRoot(docs.getAbsolutePath))
      .add(ProjectName("Scala 3 Reference"))
      .add(ProjectVersion(Versions.baseVersion))
      .remove[VersionsDictionaryUrl]
      .add(SourceLinks(List(
        s"${docs.getAbsolutePath}=github://scala/scala3/language-reference-stable#docs"
      )))
      .add(GenerateAPI(false))
      .add(SnippetCompiler(ScaladocConfigs0.referenceSnippetCompilerTargets(docs.getAbsolutePath)))

  def expectedLinksRegenerationCommand(): Seq[String] = {
    val script = (file("project") / "scripts" / "regenerateExpectedLinks").toString
    val expectedLinksFile = (file("project") / "scripts" / "expected-links" / "reference-expected-links.txt").toString
    Seq(script, referenceDocumentationOutputDir, expectedLinksFile)
  }

  abstract class SourcePatch(val file: File) {
    def apply(): Unit
    def revert(): Unit
  }

  /** Generate full sidebar.yml based on template and reference content */
  def sidebarSourcePatch(docs: File): SourcePatch =
    new SourcePatch(docs / "sidebar.yml") {
      val referenceSideBarCopy = IO.temporaryDirectory / "sidebar.yml.copy"
      IO.copyFile(file, referenceSideBarCopy)

      override def apply(): Unit = {
        val yaml = new org.yaml.snakeyaml.Yaml()
        type YamlObject = java.util.Map[String, AnyRef]
        type YamlList[T] = java.util.List[T]
        def loadYaml(file: File): YamlObject = {
          val reader = Files.newBufferedReader(file.toPath)
          try yaml.load(reader).asInstanceOf[YamlObject]
          finally reader.close()
        }
        // Ensure to always operate on original (Map, List) instances
        val template = loadYaml(docs / "sidebar.nightly.template.yml")
        template.get("subsection")
          .asInstanceOf[YamlList[YamlObject]]
          .stream()
          .filter(_.get("title") == "Reference")
          .findFirst()
          .orElseThrow(() => new IllegalStateException("Reference subsection not found in sidebar.nightly.template.yml"))
          .putAll(loadYaml(referenceSideBarCopy))

        val sidebarWriter = Files.newBufferedWriter(this.file.toPath)
        try yaml.dump(template, sidebarWriter)
        finally sidebarWriter.close()
      }
      override def revert(): Unit = IO.move(referenceSideBarCopy, file)
    }

  /** Add patch about nightly version usage */
  def nightlyVersionNoteSourcePatch(docs: File): SourcePatch =
    new SourcePatch(docs / "_layouts" / "static-site-main.html") {
      lazy val originalContent = IO.read(file)

      val warningMessage = """{% if page.nightlyOf %}
        |  <aside class="warning">
        |    <div class='icon'></div>
        |    <div class='content'>
        |      This is a nightly documentation. The content of this page may not be consistent with the current stable version of language.
        |      Click <a href="{{ page.nightlyOf }}">here</a> to find the stable version of this page.
        |    </div>
        |  </aside>
        |{% endif %}""".stripMargin

      override def apply(): Unit = {
        IO.write(file,
          originalContent
          .replace("{{ content }}", s"$warningMessage {{ content }}")
          .ensuring(_.contains(warningMessage), "patch to static-site-main layout not applied!")
        )
      }
      override def revert(): Unit = IO.write(file, originalContent)
    }
}
