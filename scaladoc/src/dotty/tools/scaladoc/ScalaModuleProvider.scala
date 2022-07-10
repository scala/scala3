package dotty.tools.scaladoc

import dotty.tools.scaladoc.tasty.ScaladocTastyInspector
import scala.jdk.CollectionConverters._
import transformers._

case class Module(rootPackage: Member, members: Map[DRI, Member])

object ScalaModuleProvider:
  def mkModule()(using ctx: DocContext): Module =
    val (result, rootDoc) = ScaladocTastyInspector().result()
    val (rootPck, rest) = result.partition(_.name == "API")
    val packageMembers = (rest ++ rootPck.flatMap(_.members))
      .filter(p => p.members.nonEmpty || p.docs.nonEmpty).sortBy(_.name)

    def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.members.flatMap(flattenMember)

    val topLevelPackage =
      Member("API", site.apiPageDRI, Kind.RootPackage, members = packageMembers, docs = rootDoc)

    val original = Module(topLevelPackage, flattenMember(topLevelPackage).toMap)

    val transformers = List(
      ImplicitMembersExtensionTransformer(),
      InheritanceInformationTransformer(),
      SealedMarksGraphTransformer()
    )
    transformers.foldLeft(original)((module, transformer) => transformer.apply(module))
