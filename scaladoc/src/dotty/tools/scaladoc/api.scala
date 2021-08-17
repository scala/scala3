package dotty.tools.scaladoc

import dotty.tools.scaladoc.tasty.comments.Comment

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
  case Deferred extends Modifier("", true)
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
  case Transparent extends Modifier("transparent", true)

case class ExtensionTarget(name: String, typeParams: Seq[TypeParameter], argsLists: Seq[ParametersList], signature: Signature, dri: DRI, position: Long)
case class ImplicitConversion(from: DRI, to: DRI)
trait ImplicitConversionProvider { def conversion: Option[ImplicitConversion] }
trait Classlike

enum Kind(val name: String):
  case RootPackage extends Kind("")
  case Package extends Kind("package")
  case Class(typeParams: Seq[TypeParameter], argsLists: Seq[ParametersList])
    extends Kind("class") with Classlike
  case Object extends Kind("object") with Classlike
  case Trait(typeParams: Seq[TypeParameter], argsLists: Seq[ParametersList])
    extends Kind("trait") with Classlike
  case Enum(typeParams: Seq[TypeParameter], argsLists: Seq[ParametersList]) extends Kind("enum") with Classlike
  case EnumCase(kind: Object.type | Type | Val.type | Class) extends Kind("case")
  case Def(typeParams: Seq[TypeParameter], argsLists: Seq[ParametersList])
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

enum Origin:
  case ImplicitlyAddedBy(name: String, dri: DRI)
  case ExtensionFrom(name: String, dri: DRI)
  case ExportedFrom(name: String, dri: Option[DRI])
  case Overrides(overriddenMembers: Seq[Overridden])
  case RegularlyDefined

case class Overridden(name: String, dri: DRI)

case class InheritedFrom(name: String, dri: DRI)

case class Annotation(val dri: DRI, val params: List[Annotation.AnnotationParameter])

object Annotation:
  sealed trait AnnotationParameter {
    val name: Option[String]
  }
  case class PrimitiveParameter(name: Option[String] = None, value: String) extends AnnotationParameter
  case class LinkParameter(name: Option[String] = None, dri: DRI, value: String) extends AnnotationParameter
  case class UnresolvedParameter(name: Option[String] = None, unresolvedText: String) extends AnnotationParameter

case class ParametersList(
  parameters: Seq[Parameter],
  modifiers: String
)

case class Parameter(
  annotations: Seq[Annotation],
  modifiers: String,
  name: Option[String],
  dri: DRI,
  signature: Signature,
  isExtendedSymbol: Boolean = false,
  isGrouped: Boolean = false
)

case class TypeParameter(
  annotations: Seq[Annotation],
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

case class HierarchyGraph(edges: Seq[(LinkToType, LinkToType)], sealedNodes: Set[LinkToType] = Set.empty):
  def vertecies: Seq[LinkToType] = edges.flatten((a, b) => Seq(a, b)).distinct
  def verteciesWithId: Map[LinkToType, Int] = vertecies.zipWithIndex.toMap
  def +(edge: (LinkToType, LinkToType)): HierarchyGraph = HierarchyGraph((edges :+ edge).distinct)
  def ++(edges: Seq[(LinkToType, LinkToType)]): HierarchyGraph = edges.foldLeft(this) {
    case (acc, edge) => acc + edge
  }
object HierarchyGraph:
  def empty = HierarchyGraph(Seq.empty)
  def withEdges(edges: Seq[(LinkToType, LinkToType)]) = HierarchyGraph.empty ++ edges

case class Member(
  name: String,
  dri: DRI,
  kind: Kind,
  visibility: Visibility = Visibility.Unrestricted,
  modifiers: Seq[Modifier] = Nil,
  annotations: List[Annotation] = Nil,
  signature: Signature = Signature(),
  sources: Option[TastyMemberSource] = None,
  origin: Origin = Origin.RegularlyDefined,
  inheritedFrom: Option[InheritedFrom] = None,
  graph: HierarchyGraph = HierarchyGraph.empty,
  docs: Option[Comment] = None,
  members : Seq[Member] = Nil,
  directParents: Seq[LinkToType] = Nil,
  parents: Seq[LinkToType] = Nil,
  selfType: Option[LinkToType] = None,
  knownChildren: Seq[LinkToType] = Nil,
  companion: Option[DRI] = None,
  deprecated: Option[Annotation] = None,
):
  def needsOwnPage: Boolean =
    def properKind(kind: Kind): Boolean = kind match
      case Kind.Package => true
      case _ if kind.isInstanceOf[Classlike] => true
      case Kind.Given(inner, _, _) => properKind(inner)
      case Kind.EnumCase(inner) => properKind(inner)
      case _ => false

    properKind(kind) &&
      origin == Origin.RegularlyDefined &&
      inheritedFrom.isEmpty

object Member:
  def unapply(v: Member): Option[(String, DRI, Visibility, Kind, Origin)] =
    Some((v.name, v.dri, v.visibility, v.kind, v.origin))

extension[T] (member: Member)
  def asLink: LinkToType = LinkToType(member.signature, member.dri, member.kind)
  def membersBy(op: Member => Boolean): Seq[Member] = member.members.filter(op)

  def withMembers(newMembers: Seq[Member]): Member = member.copy(members = newMembers)

  def updateRecusivly(op: Member => Member): Member =
    val newMembers = member.members.map(_.updateRecusivly(op))
    op(member).withMembers(newMembers)

  def withOrigin(origin: Origin): Member = member.copy(origin = origin)

  def withKind(kind: Kind): Member = member.copy(kind = kind)

  def withNewMembers(newMembers: Seq[Member]): Member =
    member.copy(members = member.members ++ newMembers)

  def withKnownChildren(knownChildren: Seq[LinkToType]): Member =
    member.copy(knownChildren = knownChildren)

  def withNewGraphEdges(edges: Seq[(LinkToType, LinkToType)]): Member =
    member.copy(graph = member.graph ++ edges)

  def getDirectParentsAsStrings: Seq[String] =
    member.directParents.map(_.signature.getName).sorted

  def getParentsAsStrings: Seq[String] =
    member.parents.map(_.signature.getName).sorted

  def getKnownChildrenAsStrings: Seq[String] =
    member.knownChildren.map(_.signature.getName).sorted

extension (members: Seq[Member]) def byInheritance =
  members.partition(_.inheritedFrom.isEmpty)

extension (m: Module)
  def updatePackages(op: Seq[Member] => Seq[Member]): Module =
    val newRoot = m.rootPackage.withMembers(op(m.rootPackage.members))
    m.copy(rootPackage = newRoot)

  def updateMembers(op: Member => Member): Module =
     updatePackages(_.map(p => p.updateRecusivly(op)))

  def visitMembers(callback: Member => Unit): Unit =
    def visitClasslike(c: Member): Unit =
      callback(c)
      c.members.foreach(visitClasslike(_))

    visitClasslike(m.rootPackage)

extension (s: Signature)
  def getName: String =
    s.map {
      case s: String => s
      case l: Link => l.name
    }.mkString

case class TastyMemberSource(path: java.nio.file.Path, lineNumber: Int)

object SnippetCompilerData:
  case class Position(line: Int, column: Int)
  case class ClassInfo(tpe: Option[String], names: Seq[String], generics: Option[String])

case class SnippetCompilerData(
  packageName: String,
  classInfos: Seq[SnippetCompilerData.ClassInfo],
  imports: List[String],
  position: SnippetCompilerData.Position
)
