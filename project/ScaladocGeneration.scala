package dotty.tools.sbtplugin

import java.nio.file.Files
import sbt._

object ScaladocGeneration {

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

  def expectedLinksRegenerationCommand(): Seq[String] = {
    val script = (file("project") / "scripts" / "regenerateExpectedLinks").toString
    val expectedLinksFile = (file("project") / "scripts" / "expected-links" / "reference-expected-links.txt").toString
    Seq(script, ScaladocConfigs0.referenceDocumentationOutputDir, expectedLinksFile)
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
