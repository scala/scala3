package dotty.tools.dottydoc
package util

object traversing {
  import model._

  def mutateEntities(e: Entity)(trans: Entity => Unit): Unit = e match {
    case e: Entity with Members =>
      trans(e)
      e.members.map(mutateEntities(_)(trans))
    case e: Entity => trans(e)
  }

  def relativePath(from: Entity, to: Entity) = {
    val offset = from match {
      case v: Val if v.implicitlyAddedFrom.isDefined => 3
      case d: Def if d.implicitlyAddedFrom.isDefined => 3
      case _: Val | _: Def => 2
      case _ => 1
    }

    "../" * (from.path.length - offset) +
    to.path.mkString("", "/", ".html")
  }


  def rootPackages(pkgs: Map[String, Package]): List[Package] = {
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
