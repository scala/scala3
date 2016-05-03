package dotty.tools.dottydoc
package util
package internal

object setters {
  import model._
  import model.comment.Comment
  import model.internal._

  def setComment(ent: Entity, to: Option[Comment]) = ent match {
    case x: PackageImpl   => x.comment = to
    case x: ClassImpl     => x.comment = to
    case x: CaseClassImpl => x.comment = to
    case x: TraitImpl     => x.comment = to
    case x: ObjectImpl    => x.comment = to
    case x: DefImpl       => x.comment = to
    case x: ValImpl       => x.comment = to
  }

  def setParent(ent: Entity, to: Entity): Unit = ent match {
    case e: ClassImpl =>
      e.parent = to
      e.members.foreach(setParent(_, e))
    case e: CaseClassImpl =>
      e.parent = to
      e.members.foreach(setParent(_, e))
    case e: ObjectImpl =>
      e.parent = to
      e.members.foreach(setParent(_, e))
    case e: TraitImpl =>
      e.parent = to
      e.members.foreach(setParent(_, e))
    case e: ValImpl =>
      e.parent = to
    case e: DefImpl =>
      e.parent = to
    case _ => ()
  }
}
