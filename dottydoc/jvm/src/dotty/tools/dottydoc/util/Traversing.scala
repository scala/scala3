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
      case _: Val | _: Def => 2
      case _ => 1
    }

    "../" * (from.path.length - offset) +
    to.path.mkString("", "/", ".html")
  }
}
