package dotty.tools.dottydoc
package util
package internal

object setters {
  import model._
  import comment.Comment
  import model.references._
  import internal._

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

  implicit class FlattenedEntity(val ent: Entity) extends AnyVal {
    /** Returns a flat copy if anything was changed (Entity with Members) else
     *  the identity
     */
    def flat: Entity = {
      def flattenMember: Entity => Entity = {
        case e: PackageImpl   => e.copy(members = Nil)
        case e: ObjectImpl    => e.copy(members = Nil)
        case e: CaseClassImpl => e.copy(members = Nil)
        case e: ClassImpl     => e.copy(members = Nil)
        case e: TraitImpl     => e.copy(members = Nil)
        case other            => other
      }

      ent match {
        case e: PackageImpl   => e.copy(members = e.members.map(flattenMember))
        case e: ObjectImpl    => e.copy(members = e.members.map(flattenMember))
        case e: CaseClassImpl => e.copy(members = e.members.map(flattenMember))
        case e: ClassImpl     => e.copy(members = e.members.map(flattenMember))
        case e: TraitImpl     => e.copy(members = e.members.map(flattenMember))
        case other            => other
      }
    }
  }
}
