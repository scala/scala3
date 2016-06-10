package dotty.tools.dottydoc
package model

import comment.{ Comment, MaterializableLink }

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
  def returnValue: MaterializableLink
}

trait Package extends Entity with Members {
  val kind = "package"

  def children: List[Entity with Members]
}

trait Class extends Entity with Members with Modifiers {
  val kind = "class"
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
  def typeParams: List[String]
  def paramLists: List[List[(String, MaterializableLink)]]
  val kind = "def"
}

trait Val extends Entity with Modifiers with ReturnValue {
  val kind = "val"
}

trait Var extends Entity with Modifiers with ReturnValue {
  val kind = "var"
}

trait NonEntity extends Entity {
  val name    = ""
  val comment = None
  val path    = Nil
  val kind    = ""
  val parent  = NonEntity
}

final case object NonEntity extends NonEntity
final case object RootEntity extends NonEntity {
  override val name = "root"
}
