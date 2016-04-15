package dotty.tools.dottydoc
package model

import comment.Comment

object Entities {
  import prickle._

  type PackageMember = Entity with Members with Modifiers

  sealed trait Entity {
    def name: String

    /** Path from root, i.e. `scala.Option$` */
    def path: List[String]

    def comment: Option[Comment]

    def sourceUrl: String = "#"

    def kind: String

    def parent: Option[Entity]

    /** All parents from package level i.e. Package to Object to Member etc */
    def parents: List[Entity] =
      parent.map(p => p :: p.parents).getOrElse(Nil)
  }

  sealed trait Members {
    def members: List[Entity]
  }

  sealed trait Modifiers {
    def modifiers: List[String]

    val isPrivate: Boolean =
      modifiers.contains("private")
  }

  final case class Package(
    name: String,
    members: List[Entity],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Members {
    override val kind = "package"

    var parent: Option[Entity] = None
    val children: List[PackageMember] =
      members.collect { case x: PackageMember => x }
  }

  final case class Class(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Members with Modifiers {
    override val kind = "class"
    var parent: Option[Entity] = None
  }

  final case class CaseClass(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Members with Modifiers {
    override val kind = "case class"
    var parent: Option[Entity] = None
  }

  final case class Trait(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Members with Modifiers {
    override val kind = "trait"
    var parent: Option[Entity] = None
  }

  final case class Object(
    name: String,
    members: List[Entity],
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Members with Modifiers {
    override val kind = "object"
    var parent: Option[Entity] = None
  }

  final case class Def(
    name: String,
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Modifiers {
    override val kind = "def"
    var parent: Option[Entity] = None
  }

  final case class Val(
    name: String,
    modifiers: List[String],
    path: List[String],
    var comment: Option[Comment] = None
  ) extends Entity with Modifiers {
    override val kind = "val"
    var parent: Option[Entity] = None
  }

  /** This object is used to represent entities that are to be filtered out */
  final case object NonEntity extends Entity {
    override val name = ""
    override val comment = None
    override val path = Nil
    override val kind = ""
    override val parent = None
  }

  final case object RootEntity extends Entity {
    override val name = "root"
    override val comment = None
    override val path = Nil
    override val kind = ""
    override val parent = None
  }

  //implicit val pMPickler: PicklerPair[PackageMember] = CompositePickler[PackageMember]
  //  .concreteType[Class]
  //  .concreteType[CaseClass]
  //  .concreteType[Object]
  //  .concreteType[Trait]

  implicit val entityPickler: PicklerPair[Entity] = CompositePickler[Entity]
    .concreteType[Val]
    .concreteType[Def]
    .concreteType[Class]
    .concreteType[CaseClass]
    .concreteType[Object]
    .concreteType[Trait]
    .concreteType[Package]
}
