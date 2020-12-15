package dotty.dokka
package model
package api

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.model.{Projection => JProjection}
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.model.DFunction
import org.jetbrains.dokka.model.DClass
import org.jetbrains.dokka.model.DocumentableSource
import org.jetbrains.dokka.model.Dynamic
import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.model.TypeConstructor
import org.jetbrains.dokka.model.TypeParameter
import org.jetbrains.dokka.model.UnresolvedBound
import org.jetbrains.dokka.model.DPackage
import org.jetbrains.dokka.model.DModule

import collection.JavaConverters._
import org.jetbrains.dokka.model.doc.DocumentationNode
import org.jetbrains.dokka.model.properties._
import dotty.dokka.tasty.comments.Comment

case class MemberExtension(
  visibility: Visibility,
  modifiers: Seq[dotty.dokka.model.api.Modifier],
  kind: Kind,
  annotations: List[Annotation],
  signature: Signature,
  sources: Option[TastyDocumentableSource] = None,
  origin: Origin = Origin.RegularlyDefined,
  inheritedFrom: Option[InheritedFrom] = None,
  graph: HierarchyGraph = HierarchyGraph.empty,
  rawDoc: Option[Comment] = None
) extends ExtraProperty[Documentable]:
 override def getKey = MemberExtension

object MemberExtension extends BaseKey[Documentable, MemberExtension]:
  val empty = MemberExtension(Visibility.Unrestricted, Nil, Kind.Unknown, Nil, Signature(), None)

case class CompositeMemberExtension(
  members : Seq[Member] = Nil,
  directParents: Seq[Signature] = Nil,
  parents: Seq[LinkToType] = Nil,
  knownChildren: Seq[LinkToType] = Nil,
  companion: Option[DRI] = None,
) extends ExtraProperty[Documentable]:
  override def getKey = CompositeMemberExtension

object CompositeMemberExtension extends BaseKey[Documentable, CompositeMemberExtension]:
  val empty = CompositeMemberExtension()

  override def mergeStrategyFor(left: CompositeMemberExtension, right: CompositeMemberExtension) =
    new MergeStrategy$Replace(left.copy(members = left.members ++ right.members))
      .asInstanceOf[MergeStrategy[Documentable]]

extension (member: Member)
  private def putInMember(ext: MemberExtension) =
    val memberWithExtra = member.asInstanceOf[WithExtraProperties[Member]]
    memberWithExtra.withNewExtras(memberWithExtra.getExtra plus ext).asInstanceOf[Member]

  private def putInCompositeMember(ext: CompositeMemberExtension) =
    val memberWithExtra = member.asInstanceOf[WithExtraProperties[Member]]
    memberWithExtra.withNewExtras(memberWithExtra.getExtra plus ext).asInstanceOf[Member]

  def copy(modifiers: Seq[Modifier]): Member =
    val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(modifiers = modifiers)
    putInMember(ext)

  def withOrigin(origin: Origin): Member =
    val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(origin = origin)
    putInMember(ext)

  def withInheritedFrom(inheritedFrom: InheritedFrom): Member =
    val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(inheritedFrom = Some(inheritedFrom))
    putInMember(ext)

  def withKind(kind: Kind): Member =
    val ext = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty).copy(kind = kind)
    putInMember(ext)

  def withMembers(newMembers: Seq[Member]): Member =
    val original = member.compositeMemberExt.getOrElse(CompositeMemberExtension())
    val newExt = original.copy(members = newMembers)
    putInCompositeMember(newExt)

  def withNewMembers(newMembers: Seq[Member]): Member =
    val original = member.compositeMemberExt.getOrElse(CompositeMemberExtension())
    val newExt = original.copy(members = original.members ++ newMembers)
    putInCompositeMember(newExt)

  def withKnownChildren(knownChildren: Seq[LinkToType]): Member =
    val original = member.compositeMemberExt.getOrElse(CompositeMemberExtension())
    val newExt = original.copy(knownChildren = knownChildren)
    putInCompositeMember(newExt)

  def withNewGraphEdges(edges: Seq[(LinkToType, LinkToType)]): Member =
    val oldExt = MemberExtension.getFrom(member).getOrElse(MemberExtension.empty)
    val newExt = oldExt.copy(graph = oldExt.graph ++ edges)
    putInMember(newExt)

  def updateRecusivly(op: Member => Member): Member =
    val newMembers = member.allMembers.map(_.updateRecusivly(op))
    op(member).withMembers(newMembers)

extension (bound: Bound)
  def asSignature: Signature = bound match
    case tc: TypeConstructor =>
      tc.getProjections.asScala.toSeq.map {
        case txt: UnresolvedBound => txt.getName
        case link: TypeParameter =>
          Link(link.getName, link.getDri)
      }

extension (m: DModule)
  def updatePackages(op: Seq[Member] => Seq[Member]): DModule =
    val topLevelPck = m.getPackages.get(0)
    val newRoot = topLevelPck.withMembers(op(topLevelPck.allMembers))

    m.copy(
        m.getName,
        JList(newRoot.asInstanceOf[DPackage]),
        m.getDocumentation,
        m.getExpectPresentInSet,
        m.getSourceSets,
        m.getExtra
    )

  def updateMembers(op: Member => Member): DModule =
     updatePackages(_.map(p => p.updateRecusivly(op)))
