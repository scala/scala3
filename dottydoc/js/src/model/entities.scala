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
trait Package extends Entity with Members

@ScalaJSDefined
trait Class extends Entity with Members with Modifiers

@ScalaJSDefined
trait CaseClass extends Class

@ScalaJSDefined
trait Object extends Class

@ScalaJSDefined
trait Trait extends Class

@ScalaJSDefined
trait Def extends Entity with Modifiers {
  val typeParams: sjs.Array[String]
  //val paramLists: sjs.Array[sjs.Array[(String, _)]]
}

@ScalaJSDefined
trait Val extends Entity with Modifiers

@ScalaJSDefined
trait Var extends Entity with Modifiers

object ops {
  val EntitiesWithMembers =
    "package" :: "case class" :: "class" :: "object" :: "trait" :: Nil

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
    }
  }

  implicit class EntityOps(val ent: Entity) {
    def typeParams: sjs.Array[String] =
      if (ent.kind == "def")
        ent.asInstanceOf[Def].typeParams
      else sjs.Array()
  }
}
