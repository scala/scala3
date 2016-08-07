package dotty.tools
package dottydoc
package js
package model

import scala.scalajs.{ js => sjs }
import sjs.annotation.ScalaJSDefined

/** This file defines the interface for which to interact with the searchable
 *  index. To use the normal operations available on the traits on the JVM:
 *
 *  {{{
 *  import dotty.tools.dottydoc.js.model.ops._
 *  val x: Package = ...
 *  }}}
 *
 *  Please note that some of the actual fields have not been added to this
 *  interface, this is simply due to the fact that they're not necessary for
 *  search - YET. They could be added, for instance `comment` is missing.
 */
@ScalaJSDefined
trait Entity extends sjs.Object {
  val kind: String
  val name: String
  val path: sjs.Array[String]
  val comment: sjs.UndefOr[Comment]
}

@ScalaJSDefined
trait Comment extends sjs.Object {
  val body: String
  val short: String
}

@ScalaJSDefined
trait Members extends sjs.Object {
  val members: sjs.Array[Entity]
}

@ScalaJSDefined
trait Modifiers extends sjs.Object {
  val modifiers: sjs.Array[String]
}

@ScalaJSDefined
trait ReturnValue extends sjs.Object {
  val returnValue: Reference
}

@ScalaJSDefined
trait TypeParams extends sjs.Object {
  val typeParams: sjs.Array[String]
}

@ScalaJSDefined
trait SuperTypes extends sjs.Object {
  val superTypes: sjs.Array[MaterializableLink]
}

@ScalaJSDefined
trait Package extends Entity with Members

@ScalaJSDefined
trait Class extends Entity with Members with Modifiers with TypeParams

@ScalaJSDefined
trait CaseClass extends Class

@ScalaJSDefined
trait Object extends Entity with Members with Modifiers

@ScalaJSDefined
trait Trait extends Class

@ScalaJSDefined
trait ParamList extends sjs.Object {
  val list: sjs.Array[NamedReference]
  val isImplicit: Boolean
}

@ScalaJSDefined
trait Def extends Entity with Modifiers with ReturnValue {
  val typeParams: sjs.Array[String]
  val paramLists: sjs.Array[ParamList]
  val implicitlyAddedFrom: sjs.UndefOr[Reference]
}

@ScalaJSDefined
trait Val extends Entity with Modifiers {
  val implicitlyAddedFrom: sjs.UndefOr[Reference]
}

@ScalaJSDefined
trait Var extends Entity with Modifiers {
  val implicitlyAddedFrom: sjs.UndefOr[Reference]
}

@ScalaJSDefined
trait ImplicitlyAddedEntity extends Entity {
  val implicitlyAddedFrom: sjs.UndefOr[Reference]
}

object ops {
  val EntitiesWithModifiers =
    "case class" :: "class" :: "object" :: "trait" :: "def" :: "val" :: Nil

  val EntitiesWithMembers =
    "package" :: "case class" :: "class" :: "object" :: "trait" :: Nil

  val EntitiesWithTypeParams =
    "case class" :: "class" :: "trait" :: "def" :: Nil

  val EntitiesWithSuperTypes =
    "case class" :: "class" :: "trait" :: "object" :: Nil

  implicit class PackageOps(val p: Package) {
    def children: sjs.Array[Entity with Members] =
      p.members.collect {
        case x if EntitiesWithMembers contains x.kind =>
          x.asInstanceOf[Entity with Members]
      }

    def withMembers(mbrs: sjs.Array[Entity]): Package = new Package {
      val kind = p.kind
      val name = p.name
      val path = p.path
      val members = mbrs
      val comment = p.comment
    }
  }

  implicit class EntityOps(val ent: Entity) {
    def typeParams: sjs.Array[String] =
      if (ent.kind == "def")
        ent.asInstanceOf[Def].typeParams
      else sjs.Array()

    def hasMembers: Boolean =
      EntitiesWithMembers contains ent.kind

    def hasModifiers: Boolean =
      EntitiesWithModifiers contains ent.kind

    def hasTypeParams: Boolean =
      EntitiesWithTypeParams contains ent.kind

    def hasSuperTypes: Boolean =
      EntitiesWithSuperTypes contains ent.kind

    def isPrivate: Boolean =
      hasModifiers &&
      ent.asInstanceOf[Modifiers].modifiers.contains("private")

    def addedImplicitly: Boolean = (ent.kind == "def" || ent.kind == "val") && {
      ent.asInstanceOf[ImplicitlyAddedEntity].implicitlyAddedFrom.isDefined
    }

    def foldImplicitlyAdded[B](f: Reference => B): sjs.UndefOr[B] =
      if (ent.kind == "def" || ent.kind == "val") ent.asInstanceOf[ImplicitlyAddedEntity].implicitlyAddedFrom.map(f)
      else sjs.undefined
  }
}
