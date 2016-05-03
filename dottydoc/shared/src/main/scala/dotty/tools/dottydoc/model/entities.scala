package dotty.tools.dottydoc
package model

import comment.Comment

trait Entity {
  def name: String

  /** Path from root, i.e. `scala.Option$` */
  def path: List[String]

  def comment: Option[Comment]

  def kind: String

  def parent: Entity

  /** All parents from package level i.e. Package to Object to Member etc */
  def parents: List[Entity] = parent match {
    case NonEntity => Nil
    case e => e :: e.parents
  }

  /** Applies `f` to entity if != `NonEntity` */
  def fold[A](nonEntity: A)(f: Entity => A) = this match {
    case NonEntity => nonEntity
    case x => f(x)
  }
}

trait Members {
  def members: List[Entity]
}

trait Modifiers {
  def modifiers: List[String]

  val isPrivate: Boolean =
    modifiers.contains("private")
}

trait ReturnValue {
  def returnValue: String
}

trait Package extends Entity with Members {
  override val kind = "package"

  def children: List[Entity with Members]
}

trait Class extends Entity with Members with Modifiers {
  override val kind = "class"
}

trait CaseClass extends Class {
  override val kind = "case class"
}

trait Trait extends Class {
  override val kind = "trait"
}

trait Object extends Class {
  override val kind = "object"
}

trait Def extends Entity with Modifiers with ReturnValue {
  override val kind = "def"
}

trait Val extends Def {
  override val kind = "val"
}

trait Var extends Def {
  override val kind = "var"
}

trait NonEntity extends Entity {
  override val name    = ""
  override val comment = None
  override val path    = Nil
  override val kind    = ""
  override val parent  = NonEntity
}

final case object NonEntity extends NonEntity
final case object RootEntity extends NonEntity {
  override val name = "root"
}
