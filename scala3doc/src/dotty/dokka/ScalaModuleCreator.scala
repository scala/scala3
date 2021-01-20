package dotty.dokka

import dotty.dokka.tasty.DokkaTastyInspector
import dotty.dokka.model.api._
import org.jetbrains.dokka.base.parsers.MarkdownParser
import collection.JavaConverters._
import kotlin.coroutines.Continuation

case class Module(rootPackage: Member, members: Map[DRI, Member])

object ScalaModuleProvider:
  def mkModule()(using ctx: DocContext): Module =
    val (result, rootDoc) = DokkaTastyInspector(new MarkdownParser(_ => null)).result()
    val (rootPck, rest) = result.partition(_.name == "API")
    val packageMembers = (rest ++ rootPck.flatMap(_.members)).sortBy(_.name)

    def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.members.flatMap(flattenMember)

    val topLevelPackage =
      Member("API", site.apiPageDRI, Kind.RootPackage, members = packageMembers, docs = rootDoc)

    val original = Module(topLevelPackage, flattenMember(topLevelPackage).toMap)

    val transformers = List(
      ImplicitMembersExtensionTransformer(),
      InheritanceInformationTransformer()
    )
    transformers.foldLeft(original)((module, transformer) => transformer.apply(module))
