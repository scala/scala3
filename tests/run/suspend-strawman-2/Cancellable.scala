package concurrent
import scala.collection.mutable

/** A trait for cancellable entities that can be grouped */
trait Cancellable:

  private var group: Cancellable.Group = Cancellable.Unlinked

  /** Issue a cancel request */
  def cancel(): Unit

  /** Add this cancellable to the given group after removing
   *  it from the previous group in which it was.
   */
  def link(group: Cancellable.Group): this.type =
    this.group.drop(this)
    this.group = group
    this.group.add(this)
    this

  /** Link this cancellable to the cancellable group of the
   *  current async context.
   */
  def link()(using async: Async): this.type =
    link(async.config.group)

  /** Unlink this cancellable from its group. */
  def unlink(): this.type =
    link(Cancellable.Unlinked)

object Cancellable:

  /** A group of cancellable members */
  class Group extends Cancellable:
    private var members: mutable.Set[Cancellable] = mutable.Set()

    /** Cancel all members and clear the members set */
    def cancel() =
      members.toArray.foreach(_.cancel())
      members.clear()

    /** Add given member to the members set */
    def add(member: Cancellable): Unit = synchronized:
      members += member

    /** Remove given member from the members set if it is an element */
    def drop(member: Cancellable): Unit = synchronized:
      members -= member
  end Group

  /** A sentinal group of cancellables that are in fact not linked
   *  to any real group. `cancel`, `add`, and `drop` do nothing when
   *  called on this group.
   */
  object Unlinked extends Group:
    override def cancel() = ()
    override def add(member: Cancellable): Unit = ()
    override def drop(member: Cancellable): Unit = ()
  end Unlinked

end Cancellable

