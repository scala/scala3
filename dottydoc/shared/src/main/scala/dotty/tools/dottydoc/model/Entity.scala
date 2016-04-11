package dotty.tools.dottydoc
package model

import comment.Comment

object Entities {
  import prickle._

  type PackageMember = Entity with Members with Modifiers

  sealed trait Entity {
    def name: String

    def path: List[String]

    def comment: Option[Comment]

    val sourceUrl: String = "#"

    val kind: String
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
    comment: Option[Comment],
    path: List[String]
  ) extends Entity {
    override val kind = "package"

    val children: List[PackageMember] =
      members.collect { case x: PackageMember => x }
  }

  final case class Class(
    name: String,
    members: List[Entity],
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Members with Modifiers {
    override val kind = "class"
  }

  final case class CaseClass(
    name: String,
    members: List[Entity],
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Members with Modifiers {
    override val kind = "case class"
  }

  final case class Trait(
    name: String,
    members: List[Entity],
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Members with Modifiers {
    override val kind = "trait"
  }

  final case class Object(
    name: String,
    members: List[Entity],
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Members with Modifiers {
    override val kind = "object"
  }

  final case class Def(
    name: String,
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Modifiers {
    override val kind = "def"
  }

  final case class Val(
    name: String,
    comment: Option[Comment],
    modifiers: List[String],
    path: List[String]
  ) extends Entity with Modifiers {
    override val kind = "val"
  }

  /** This object is used to represent entities that are to be filtered out */
  final case object NonEntity extends Entity {
    override val name = ""
    override val comment = None
    override val path = Nil
    override val kind = ""
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
