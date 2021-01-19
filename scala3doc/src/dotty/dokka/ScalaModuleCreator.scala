package dotty.dokka

import org.jetbrains.dokka.{ DokkaConfiguration$DokkaSourceSet => DokkaSourceSet }
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.transformers.sources.SourceToDocumentableTranslator

import dotty.dokka.tasty.DokkaTastyInspector
import org.jetbrains.dokka.pages.{Kind => _, _}
import dotty.dokka.model.api._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.base.parsers.MarkdownParser
import collection.JavaConverters._
import kotlin.coroutines.Continuation

class ScalaModuleProvider(using ctx: DocContext) extends SourceToDocumentableTranslator:
   override def invoke(sourceSet: DokkaSourceSet, cxt: DokkaContext, unused: Continuation[? >: DModule]) =
    val (result, rootDoc) = DokkaTastyInspector(new MarkdownParser(_ => null)).result()
    val (rootPck, rest) = result.partition(_.name == "<empty>")
    val packageMembers = (rest ++ rootPck.flatMap(_.allMembers)).sortBy(_.name)

    def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.allMembers.flatMap(flattenMember)

    val topLevelPackage = new DPackage(
      DRI(location = "<empty>"),
      JNil,
      JNil,
      JNil,
      JNil,
      JMap(),
      null,
      JSet(ctx.sourceSet),
      PropertyContainer.Companion.empty()
    ).withNewMembers(packageMembers).withKind(Kind.RootPackage).withDocs(rootDoc).asInstanceOf[DPackage]

    val transformers = List(
      ImplicitMembersExtensionTransformer(),
      InheritanceInformationTransformer()
    )

    val module = new DModule(
      sourceSet.getDisplayName,
      JList(topLevelPackage),
      JMap(),
      null,
      sourceSet.toSet,
      PropertyContainer.Companion.empty() plus ModuleExtension(flattenMember(topLevelPackage).toMap)
    )

    transformers.foldLeft(module)( (module, transformer) => transformer(module) )

object EmptyModuleProvider extends SourceToDocumentableTranslator:
   override def invoke(sourceSet: DokkaSourceSet, cxt: DokkaContext, unused: Continuation[? >: DModule]) =
    DModule("", JList(), Map.empty.asJava, null, Set(sourceSet).asJava, PropertyContainer.Companion.empty())
