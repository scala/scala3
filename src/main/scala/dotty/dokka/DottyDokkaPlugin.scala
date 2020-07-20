package dotty.dokka

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import dokka.java.api._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer


class DottyDokkaPlugin extends JavaDokkaPlugin:
  override def createSourceToDocumentableTranslator(cxt: DokkaContext, sourceSet: SourceSetWrapper): DModule = cxt.getConfiguration match {
    case dottyConfig: DottyDokkaConfig =>
      val parser = new MarkdownParser(null, null, cxt.getLogger)
      
      val doc = parser.parse("## THIS IS MY DOC!")

      val reader = DottyReader(sourceSet, parser, dottyConfig)

      val packages = dottyConfig.compilationUnit.packages.map(reader.parsePackage).toList

      val res = new DModule(
        sourceSet.getSourceSet.getSourceSetID.getModuleName,
        packages.asJava,
        Map().asJava,
        null,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )

      println(res)
      res
    case _ =>
      ???
  }