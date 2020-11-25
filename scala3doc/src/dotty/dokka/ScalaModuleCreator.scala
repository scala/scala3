package dotty.dokka

import org.jetbrains.dokka.{ DokkaConfiguration$DokkaSourceSet => DokkaSourceSet }
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.transformers.sources.SourceToDocumentableTranslator

import dotty.dokka.tasty.DokkaTastyInspector
import org.jetbrains.dokka.pages._
import dotty.dokka.model.api._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.base.parsers.MarkdownParser
import collection.JavaConverters._
import kotlin.coroutines.Continuation

object ScalaModuleProvider extends SourceToDocumentableTranslator:
   override def invoke(sourceSet: DokkaSourceSet, cxt: DokkaContext, unused: Continuation[? >: DModule]) =
    cxt.getConfiguration match
      case dottyConfig: DottyDokkaConfig =>
        val parser = new MarkdownParser(_ => null)
        val inspector = DokkaTastyInspector(sourceSet, parser, dottyConfig)
        val filePaths = dottyConfig.args.tastyFiles.map(_.getAbsolutePath)
        val (tastyFiles, jars) = filePaths.toList.partition(_.endsWith(".tasty"))
        val classpath = dottyConfig.args.classpath.split(java.io.File.pathSeparator).toList

        inspector.inspectAllTastyFiles(tastyFiles, jars, classpath)
        val result = inspector.result()

        def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.allMembers.flatMap(flattenMember)

        new DModule(
          sourceSet.getDisplayName,
          result.asJava,
          JMap(),
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty() plus ModuleExtension(result.flatMap(flattenMember).toMap)
        )
      case _ =>
        ???

object EmptyModuleProvider extends SourceToDocumentableTranslator:
   override def invoke(sourceSet: DokkaSourceSet, cxt: DokkaContext, unused: Continuation[? >: DModule]) =
    DModule("", JList(), Map.empty.asJava, null, Set(sourceSet).asJava, PropertyContainer.Companion.empty())
