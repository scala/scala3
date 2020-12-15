package dotty.dokka
package model
package api

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.pages._
import dotty.dokka.tasty.comments.Comment

enum Visibility(val name: String):
  case Unrestricted extends Visibility("")
  case Protected(scope: VisibilityScope) extends Visibility("protected")
  case Private(scope: VisibilityScope) extends Visibility("private")

  def asSignature = this match
    case Unrestricted => ""
    case Protected(scope) => s"protected${visibilityScopeToString(scope)}"
    case Private(scope) => s"private${visibilityScopeToString(scope)}"


  private def visibilityScopeToString(scope: VisibilityScope) = scope match
    case VisibilityScope.ImplicitTypeScope | VisibilityScope.ImplicitModuleScope => ""
    case VisibilityScope.ExplicitTypeScope(name) => s"[$name]"
    case VisibilityScope.ExplicitModuleScope(name) => s"[$name]"
    case VisibilityScope.ThisScope => "[this]"

enum VisibilityScope:
  case ImplicitTypeScope // private/protected inside a class or a trait
  case ImplicitModuleScope // private/protected inside a package or an object
  case ExplicitTypeScope(typeName: String) // private[X]/protected[X] inside a class or a trait
  case ExplicitModuleScope(moduleName: String) // private[X]/protected[X] inside a package or an object
  case ThisScope // private[this]/protected[this]

enum Modifier(val name: String, val prefix: Boolean):
  case Abstract extends Modifier("abstract", true)
  case Final extends Modifier("final", true)
  case Empty extends Modifier("", true)
  case Sealed extends Modifier("sealed", true)
  case Case extends Modifier("case", false)
  case Implicit extends Modifier("implicit", true)
  case Inline extends Modifier("inline", true)
  case Lazy extends Modifier("lazy", true)
  case Override extends Modifier("override", true)
  case Erased extends Modifier("erased", true)
  case Opaque extends Modifier("opaque", true)
  case Open extends Modifier("open", true)

case class ExtensionTarget(name: String, signature: Signature, dri: DRI, position: Long)
case class ImplicitConversion(from: DRI, to: DRI)
trait ImplicitConversionProvider { def conversion: Option[ImplicitConversion] }
trait Classlike

enum Kind(val name: String){
  case RootPackage extends Kind("")
  case Package extends Kind("package")
  case Class(typeParams: Seq[TypeParameter], argsLists: Seq[Seq[Parameter]])
    extends Kind("class") with Classlike
  case Object extends Kind("object") with Classlike
  case Trait(typeParams: Seq[TypeParameter], argsLists: Seq[Seq[Parameter]])
    extends Kind("trait") with Classlike
  case Enum extends Kind("enum") with Classlike
  case EnumCase(kind: Object.type | Type | Val.type) extends Kind("case")
  case Def(typeParams: Seq[TypeParameter], argsLists: Seq[Seq[Parameter]])
    extends Kind("def")
  case Extension(on: ExtensionTarget, m: Kind.Def) extends Kind("def")
  case Constructor(base: Kind.Def) extends Kind("def")
  case Var extends Kind("var")
  case Val extends Kind("val")
  case Exported(m: Kind.Def) extends Kind("export")
  case Type(concreate: Boolean, opaque: Boolean, typeParams: Seq[TypeParameter])
    extends Kind("type") // should we handle opaque as modifier?
  case Given(kind: Def | Class, as: Option[Signature], conversion: Option[ImplicitConversion])
    extends Kind("given") with ImplicitConversionProvider
  case Implicit(kind: Kind.Def | Kind.Val.type, conversion: Option[ImplicitConversion])
    extends Kind(kind.name)  with ImplicitConversionProvider
  case Unknown extends Kind("Unknown")
}

enum Origin:
  case ImplicitlyAddedBy(name: String, dri: DRI)
  case ExtensionFrom(name: String, dri: DRI)
  case ExportedFrom(name: String, dri: Option[DRI])
  case Overrides(overridenMembers: Seq[Overriden])
  case RegularlyDefined

case class Overriden(name: String, dri: DRI)

case class InheritedFrom(name: String, dri: DRI)

case class Annotation(val dri: DRI, val params: List[Annotation.AnnotationParameter])

object Annotation:
  sealed trait AnnotationParameter {
    val name: Option[String]
  }
  case class PrimitiveParameter(name: Option[String] = None, value: String) extends AnnotationParameter
  case class LinkParameter(name: Option[String] = None, dri: DRI, value: String) extends AnnotationParameter
  case class UnresolvedParameter(name: Option[String] = None, unresolvedText: String) extends AnnotationParameter

case class Parameter(
  annotations: Seq[Annotation],
  modifiers: String,
  name: String,
  dri: DRI,
  signature: Signature,
  isExtendedSymbol: Boolean = false,
  isGrouped: Boolean = false
)

case class TypeParameter(
  variance: "" | "+" | "-",
  name: String,
  dri: DRI,
  signature: Signature
)

// TODO (longterm) properly represent signatures
case class Link(name: String, dri: DRI)
type Signature = Seq[String | Link]

object Signature:
  def apply(names: (String | Link)*): Signature = names // TO batter dotty shortcommings in union types

extension (s: Signature)
  def join(a: Signature): Signature = s ++ a

case class LinkToType(signature: Signature, dri: DRI, kind: Kind)
case class HierarchyGraph(edges: Seq[(LinkToType, LinkToType)]):
  def vertecies: Seq[LinkToType] = edges.flatten((a, b) => Seq(a, b)).distinct
  val verteciesWithId: Map[LinkToType, Int] = vertecies.zipWithIndex.toMap
  def +(edge: (LinkToType, LinkToType)): HierarchyGraph = HierarchyGraph((edges :+ edge).distinct)
  def ++(edges: Seq[(LinkToType, LinkToType)]): HierarchyGraph = edges.foldLeft(this) {
    case (acc, edge) => acc + edge
  }
object HierarchyGraph:
  def empty = HierarchyGraph(Seq.empty)
  def withEdges(edges: Seq[(LinkToType, LinkToType)]) = HierarchyGraph.empty ++ edges


type Member = Documentable

object Member:
  def unapply(d: Documentable): Option[(String, DRI, Visibility, Kind, Origin)] =
    d.memberExt.map(v => (d.getName, d.getDri, v.visibility, v.kind, v.origin))

extension[T] (member: Member)

  private[api] def memberExt = MemberExtension.getFrom(member)

  private[api] def compositeMemberExt = CompositeMemberExtension.getFrom(member)

  def visibility: Visibility = memberExt.fold(Visibility.Unrestricted)(_.visibility)

  def signature: Signature = memberExt.fold(Signature(name))(_.signature)
  def asLink: LinkToType = LinkToType(signature, dri, kind)
  def deprecated: Option[Annotation] = memberExt.flatMap(_.annotations.find(a => a.dri.getPackageName == "scala" && a.dri.getClassNames == "deprecated"))

  def modifiers: Seq[dotty.dokka.model.api.Modifier] = memberExt.fold(Nil)(_.modifiers)
  def kind: Kind = memberExt.fold(Kind.Unknown)(_.kind)
  def origin: Origin =  memberExt.fold(Origin.RegularlyDefined)(_.origin)
  def inheritedFrom: Option[InheritedFrom] = memberExt.fold(None)(_.inheritedFrom)
  def annotations: List[Annotation] = memberExt.fold(Nil)(_.annotations)
  def sources: Option[TastyDocumentableSource] = memberExt.fold(None)(_.sources)
  def docs: Option[Comment] = memberExt.fold(None)(_.rawDoc)
  def name = member.getName
  def dri = member.getDri

  // TODO rename parent and knownChildren
  def allMembers: Seq[Member] = compositeMemberExt.fold(Nil)(_.members)
  def parents: Seq[LinkToType] = compositeMemberExt.fold(Nil)(_.parents)
  def directParents: Seq[Signature] = compositeMemberExt.fold(Nil)(_.directParents)
  def knownChildren: Seq[LinkToType] = compositeMemberExt.fold(Nil)(_.knownChildren)
  def companion: Option[DRI] = compositeMemberExt.fold(None)(_.companion)

  def membersBy(op: Member => Boolean): Seq[Member] = allMembers.filter(op)

extension (members: Seq[Member]) def byInheritance =
  members.partition(_.inheritedFrom.isEmpty)

extension (module: DModule)
  def driMap: Map[DRI, Member] = ModuleExtension.getFrom(module).fold(Map.empty)(_.driMap)

case class TastyDocumentableSource(val path: String, val lineNumber: Int)

type DocPart = DocTag
