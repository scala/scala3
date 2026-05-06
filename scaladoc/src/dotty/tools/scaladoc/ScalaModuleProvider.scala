package dotty.tools.scaladoc

import dotty.tools.scaladoc.tasty.ScaladocTastyInspector
import scala.jdk.CollectionConverters._
import transformers._

case class Module(rootPackage: Member, members: Map[DRI, Member])

object ScalaModuleProvider:
  def mkModule()(using ctx: DocContext): Module =
    if !ctx.args.generateApi then
      // Return empty module when API generation is disabled
      val emptyRoot = Member("", "", DRI(location = ""), Kind.RootPackage, members = Nil, docs = None)
      return Module(emptyRoot, Map.empty)

    val (result, rootDoc) = ScaladocTastyInspector.loadDocs()
    val (rootPck, rest) = result.partition(_.name == "API")
    val (emptyPackages, nonemptyPackages) = (rest ++ rootPck.flatMap(_.members))
      .filter(p => p.members.nonEmpty || p.docs.nonEmpty).sortBy(_.name)
      .partition(_.name == "<empty>")

    val groupedMembers =
      def groupMembers(ms: List[Member], n: Int = 0): List[Member] =
        ms.groupBy(_.name.split('.')(n)).values.map {
          case m :: ms if m.name.count(_ == '.') == n =>
            m.withMembers(groupMembers(ms, n + 1) ++ m.members)
          case ms =>
            groupMembers(ms, n + 1) match
              case m :: Nil => m
              case ms =>
                val name = ms.head.name.split('.').take(n + 1).mkString(".")
                Member(
                  name = name,
                  fullName = name,
                  dri = DRI(location = name),
                  kind = Kind.Package,
                  members = ms,
                )
        }.toList.sortBy(_.name)
      groupMembers(nonemptyPackages).reverse

    val packageMembers = groupedMembers ++ emptyPackages.flatMap(_.members)

    def flattenMember(m: Member): Seq[(DRI, Member)] = (m.dri -> m) +: m.members.flatMap(flattenMember)

    val topLevelPackage =
      Member("API", "", site.apiPageDRI, Kind.RootPackage, members = packageMembers, docs = rootDoc)

    val original = Module(topLevelPackage, flattenMember(topLevelPackage).toMap)

    val transformers = List(
      ImplicitMembersExtensionTransformer(),
      InheritanceInformationTransformer(),
      SealedMarksGraphTransformer()
    )
    transformers.foldLeft(original)((module, transformer) => transformer.apply(module))
