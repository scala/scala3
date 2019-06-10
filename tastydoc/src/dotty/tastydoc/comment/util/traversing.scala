package dotty.tastydoc
package comment
package util

import representations._

object traversing {

  def mutateEntities(e: Representation)(trans: Representation => Unit): Unit = e match {
    case e: Representation with Members =>
      trans(e)
      e.members.map(mutateEntities(_)(trans))
    case e: Representation => trans(e)
  }

  def relativePath(from: Representation, to: Representation) = {
    val offset = from match {
      case _: ValRepresentation | _: DefRepresentation => 1
      case _ => 0
    }

    "../" * (from.path.length - offset) +
    (to match {
      case r => (to.path :+ to.name).mkString("/")
    })
  }


  def rootPackages(pkgs: Map[String, PackageRepresentation]): List[PackageRepresentation] = {
    var currentDepth = Int.MaxValue
    var packs = List.empty[String]

    for (key <- pkgs.keys) {
      val keyDepth = key.split("\\.").length
      packs =
        if (keyDepth < currentDepth) {
          currentDepth = keyDepth
          key :: Nil
        } else if (keyDepth == currentDepth) {
          key :: packs
        } else packs
    }
    packs.map(pkgs.apply)
  }
}
