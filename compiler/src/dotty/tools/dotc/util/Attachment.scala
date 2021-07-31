package dotty.tools.dotc.util

import dotty.tools.dotc.core.Contexts.Context

/** A class inheriting from Attachment.Container supports
 *  adding, removing and lookup of attachments. Attachments are typed key/value pairs.
 *
 *  Attachments whose key is an instance of `StickyKey` will be kept when the attachments
 *  are copied using `withAttachmentsFrom`.
 */
object Attachment {
  import Property.{Key, StickyKey}

  /** An implementation trait for attachments.
   *  Clients should inherit from Container instead.
   */
  trait LinkSource {
    private[Attachment] var next: Link[?]

    /** Optionally get attachment corresponding to `key` */
    final def getAttachment[V](key: Key[V]): Option[V] = {
      val nx = next
      if (nx == null) None
      else if (nx.key eq key) Some(nx.value.asInstanceOf[V])
      else nx.getAttachment[V](key)
    }

    /** Does an attachment corresponding to `key` exist? */
    final def hasAttachment[V](key: Key[V]): Boolean = {
      val nx = next
      if (nx == null) false
      else if (nx.key eq key) true
      else nx.hasAttachment[V](key)
    }

    /** The attachment corresponding to `key`.
     *  @throws NoSuchElementException  if no attachment with key exists
     */
    final def attachment[V](key: Key[V]): V = {
      val nx = next
      if (nx == null) throw new NoSuchElementException
      else if (nx.key eq key) nx.value.asInstanceOf[V]
      else nx.attachment(key)
    }

    /** The attachment corresponding to `key`, or `default`
     *  if no attachment with `key` exists.
     */
    final def attachmentOrElse[V](key: Key[V], default: V): V = {
      val nx = next
      if (nx == null) default
      else if (nx.key eq key) nx.value.asInstanceOf[V]
      else nx.attachmentOrElse(key, default)
    }

    /** Add attachment with given `key` and `value`.
     *  @return  Optionally, the old attachment with given `key` if one existed before.
     *  The new attachment is added at the position of the old one, or at the end
     *  if no attachment with same `key` existed.
     */
    final def putAttachment[V](key: Key[V], value: V): Option[V] = {
      val nx = next
      if (nx == null) {
        next = new Link(key, value, null)
        None
      }
      else if (nx.key eq key) {
        next = new Link(key, value, nx.next)
        Some(nx.value.asInstanceOf[V])
      }
      else nx.putAttachment(key, value)
    }

    /** Remove attachment with given `key`, if it exists.
     *  @return  Optionally, the removed attachment with given `key` if one existed before.
     */
    final def removeAttachment[V](key: Key[V]): Option[V] = {
      val nx = next
      if (nx == null)
        None
      else if (nx.key eq key) {
        next = nx.next
        Some(nx.value.asInstanceOf[V])
      }
      else nx.removeAttachment(key)
    }

    /** The list of all keys and values attached to this container. */
    final def allAttachments: List[(Key[?], Any)] = {
      val nx = next
      if (nx == null) Nil else (nx.key, nx.value) :: nx.allAttachments
    }
  }

  /** A private, concrete implementation class linking attachments.
   */
  private[Attachment] class Link[+V](val key: Key[V], val value: V, var next: Link[?])
      extends LinkSource

  /** A trait for objects that can contain attachments */
  trait Container extends LinkSource {
    private[Attachment] var next: Link[?] = null

    /** Copy the sticky attachments from `container` to this container. */
    final def withAttachmentsFrom(container: Container): this.type = {
      var current: Link[?] = container.next
      while (current != null) {
        if (current.key.isInstanceOf[StickyKey[?]]) putAttachment(current.key, current.value)
        current = current.next
      }
      this
    }

    def withAttachment[V](key: Key[V], value: V): this.type = {
      putAttachment(key, value)
      this
    }

    final def pushAttachment[V](key: Key[V], value: V)(using ctx: Context): Unit = {
      assert(!hasAttachment(key) || ctx.run.errorsToBeReported, s"duplicate attachment for key $key")
      next = new Link(key, value, next)
    }

    final def removeAllAttachments(): Unit =
      next = null
  }
}
