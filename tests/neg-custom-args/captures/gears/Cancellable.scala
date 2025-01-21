package gears.async

import language.experimental.captureChecking

/** A trait for cancellable entities that can be grouped. */
trait Cancellable:
  private var group: CompletionGroup = CompletionGroup.Unlinked

  /** Issue a cancel request */
  def cancel(): Unit

  /** Add this cancellable to the given group after removing it from the previous group in which it was.
    */
  def link(group: CompletionGroup): this.type = synchronized:
    this.group.drop(this.unsafeAssumePure)
    this.group = group
    this.group.add(this.unsafeAssumePure)
    this

  /** Link this cancellable to the cancellable group of the current async context.
    */
  def link()(using async: Async): this.type =
    link(async.group)

  /** Unlink this cancellable from its group. */
  def unlink(): this.type =
    link(CompletionGroup.Unlinked)

  /** Assume that the [[Cancellable]] is pure, in the case that cancellation does *not* refer to captured resources.
    */
  inline def unsafeAssumePure: Cancellable = caps.unsafe.unsafeAssumePure(this)

end Cancellable

object Cancellable:
  /** A special [[Cancellable]] object that just tracks whether its linked group was cancelled. */
  trait Tracking extends Cancellable:
    def isCancelled: Boolean

  object Tracking:
    def apply() = new Tracking:
      private var cancelled: Boolean = false

      def cancel(): Unit =
        cancelled = true

      def isCancelled = cancelled
end Cancellable
