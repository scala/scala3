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

      val pck = new DPackage(
        new DRI("foo", null, null, PointingToDeclaration.INSTANCE, null),
        Nil.asJava,
        Nil.asJava,
        List(DottyReader.parseClasslike("foo.bar.Foo", sourceSet, parser)).asJava,
        Nil.asJava,
        sourceSet.asMap(doc),
        null,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )

      val res = new DModule(
        sourceSet.getSourceSet.getSourceSetID.getModuleName,
        List(pck).asJava,
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