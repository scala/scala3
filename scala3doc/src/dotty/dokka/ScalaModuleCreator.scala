package dotty.dokka

import org.jetbrains.dokka.{ DokkaConfiguration$DokkaSourceSet => DokkaSourceSet }
import com.virtuslab.dokka.site.JavaSourceToDocumentableTranslator
import com.virtuslab.dokka.site.SourceSetWrapper
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties.PropertyContainer

import dotty.dokka.tasty.{DokkaTastyInspector, SbtDokkaTastyInspector}
import org.jetbrains.dokka.pages._
import dotty.dokka.model.api._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links.DRI
import java.util.{List => JList}
import org.jetbrains.dokka.base.parsers.MarkdownParser
import collection.JavaConverters._

object ScalaModuleProvider extends JavaSourceToDocumentableTranslator:
   override def process(rawSourceSet: DokkaSourceSet, cxt: DokkaContext) =
    val sourceSet = SourceSetWrapper(rawSourceSet)
    cxt.getConfiguration match
      case dottyConfig: DottyDokkaConfig =>
        val result = dottyConfig.docConfiguration match {
          case DocConfiguration.Standalone(args, tastyFiles, jars) =>
            // TODO use it to resolve link logic
            val inspector = DokkaTastyInspector(sourceSet, new MarkdownParser(_ => null), dottyConfig)
            inspector.inspectAllTastyFiles(tastyFiles, jars, args.classpath.split(java.io.File.pathSeparator).toList)
            inspector.result()
          case DocConfiguration.Sbt(args, tastyFiles, rootCtx) =>
            val inspector =
              SbtDokkaTastyInspector(
                sourceSet,
                //   new MarkdownParser(null, null, cxt.getLogger),
                dottyConfig,
                tastyFiles,
                rootCtx,
              )
            inspector.run()
        }

        def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.allMembers.flatMap(flattenMember)

        new DModule(
          sourceSet.getSourceSet.getDisplayName,
          result.asJava,
          Map().asJava,
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty() plus ModuleExtension(result.flatMap(flattenMember).toMap)
        )
      case _ =>
        ???

object EmptyModuleProvider extends JavaSourceToDocumentableTranslator:
  override def process(sourceSet: DokkaSourceSet, context: DokkaContext) =
    DModule("", Nil.asJava, Map.empty.asJava, null, Set(sourceSet).asJava, PropertyContainer.Companion.empty())
