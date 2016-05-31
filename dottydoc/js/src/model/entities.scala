package dotty.tools
package dottydoc
package js
package model

import scala.scalajs.{ js => sjs }

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
@sjs.native
trait Entity extends sjs.Any {
  val kind: String = sjs.native

  val name: String = sjs.native

  val path: sjs.Array[String] = sjs.native

  val parent: Entity = sjs.native
}

@sjs.native
trait Members extends sjs.Any {
  val members: sjs.Array[Entity] = sjs.native
}

@sjs.native
trait Package extends Entity with Members

@sjs.native
trait Class extends Entity with Members

@sjs.native
trait CaseClass extends Entity with Members

@sjs.native
trait Object extends Class

@sjs.native
trait Trait extends Class

@sjs.native
trait Def extends Entity

@sjs.native
trait Val extends Def

@sjs.native
trait Var extends Def

object ops {
  val EntitiesWithMembers =
    "package" :: "case class" :: "class" :: "object" :: "trait" :: Nil

  implicit class PackageOps(val p: Package) {
    def children: sjs.Array[Entity with Members] =
      p.members.collect {
        case x if EntitiesWithMembers contains x.kind =>
          x.asInstanceOf[Entity with Members]
      }
  }
}
