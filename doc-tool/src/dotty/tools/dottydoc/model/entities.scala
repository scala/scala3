package dotty.tools.dottydoc
package model

import comment._
import references._
import dotty.tools.dotc.core.Symbols.{ Symbol, NoSymbol }

trait Entity { entity =>
  def symbol: Symbol

  def name: String

  /** Path from root, i.e. `scala.Option$` */
  def path: List[String]

  def comment: Option[Comment]

  def kind: String

  def parent: Entity

  def annotations: List[String]

  def signature: String =
    entity.name + (entity match {
      case o: Object => "$"
      case d: Def => d.paramLists.mkString
      case _ => ""
    })

  def children: List[Entity with Members] = entity match {
    case e: Entity with Members =>
      e.members.collect { case e: Entity with Members if e.kind != "package" => e }
    case _ => Nil
  }

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

trait SuperTypes {
  def superTypes: List[MaterializableLink]
}

trait Members {
  def members: List[Entity]

  def hasVisibleMembers: Boolean = members.exists {
    case e: Entity with Modifiers => !(e.isPrivate || e.isProtected)
    case e => true
  }
}

trait Modifiers {
  def modifiers: List[String]

  def isPrivate: Boolean =
    modifiers.contains("private")

  def isProtected: Boolean =
    modifiers.contains("protected")
}

trait TypeParams {
  def typeParams: List[String]
}

trait ReturnValue {
  def returnValue: Reference
}

trait ParamList {
  def list: List[NamedReference]
  def isImplicit: Boolean

  override def toString = list.map(_.title).mkString("(", ",", ")")
}

trait Constructors {
  def constructors: List[List[ParamList]]
}

trait Companion extends Entity {
  def hasCompanion: Boolean = companionPath ne Nil

  def companionPath: List[String]

  def companionPath_=(xs: List[String]): Unit
}

trait ImplicitlyAddedEntity extends Entity {
  def implicitlyAddedFrom: Option[Reference]
}

trait Package extends Entity with Members with SuperTypes {
  val kind = "package"
}

trait TypeAlias extends Entity with Modifiers with TypeParams {
  val kind = "type"
  def alias: Option[Reference]
  def isAbstract: Boolean = !alias.isDefined
}

trait Class extends Entity with Modifiers with TypeParams with Constructors with SuperTypes with Members with Companion {
  val kind = "class"
}

trait CaseClass extends Entity with Modifiers with TypeParams with Constructors with SuperTypes with Members with Companion {
  override val kind = "case class"
}

trait Trait extends Entity with Modifiers with TypeParams with SuperTypes with Members with Companion {
  def traitParams: List[ParamList]
  override val kind = "trait"
}

trait Object extends Entity with Modifiers with SuperTypes with Members with Companion {
  override val kind = "object"
}

trait Def extends Entity with Modifiers with TypeParams with ReturnValue with ImplicitlyAddedEntity {
  val kind = "def"
  def paramLists: List[ParamList]
}

trait Val extends Entity with Modifiers with ReturnValue with ImplicitlyAddedEntity

sealed trait NonEntity extends Package with TypeAlias with Class with CaseClass with Trait with Object with Def with Val {
  override val kind = ""
  val annotations = Nil
  val name = ""
  val symbol = NoSymbol
  val comment = None
  val path = Nil
  val parent = NonEntity
  val constructors = Nil
  val paramLists = Nil
  val implicitlyAddedFrom = None
  val members = Nil
  val modifiers = Nil
  val reference = EmptyReference
  val returnValue = EmptyReference
  val superTypes = Nil
  val typeParams = Nil
  val traitParams = Nil
  val alias = None
  val companionPath = Nil
  def companionPath_=(xs: List[String]) = ()
}

final case object NonEntity extends NonEntity
final case object RootEntity extends NonEntity {
  override val name = "root"
}
