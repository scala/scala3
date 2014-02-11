package dotty.tools.dotc.util

/** A class inheriting from Attachment.Container supports
 *  adding, removing and lookup of attachments. Attachments are typed key/value pairs.
 */
object Attachment {

  class Key[+V]

  val NullKey = new Key[Null]

  abstract class AttachmentLink[+V] extends DotClass {
    private[Attachment] def key: Key[V]
    private[Attachment] def value: V
    private[Attachment] var next: Link[_]

    def getAttachment[V](key: Key[V]): Option[V] =
      if (this.key eq key) Some(value.asInstanceOf[V])
      else if (next == null) None
      else next.getAttachment(key)

    def attachment[V](key: Key[V]): V =
      if (this.key eq key) value.asInstanceOf[V]
      else if (next == null) throw new NoSuchElementException
      else next.attachment(key)

    def attachmentOrElse[V](key: Key[V], default: V): V =
      if (this.key eq key) value.asInstanceOf[V]
      else if (next == null) default
      else next.attachmentOrElse(key, default)

    def pushAttachment[V](key: Key[V], value: V): Unit = {
      assert(!getAttachment(key).isDefined)
      next = new Link(key, value, next)
    }

    def putAttachment[V](key: Key[V], value: V): Option[V] = {
      if (next == null) {
        next = new Link(key, value, null)
        None
      }
      else if (next.key eq key) {
        val nx = next
        next = new Link(key, value, nx.next)
        Some(nx.value.asInstanceOf[V])
      }
      else next.putAttachment(key, value)
    }

    def removeAttachment[V](key: Key[V]): Option[V] = {
      if (next == null)
        None
      else if (next.key eq key) {
        val nx = next
        next = nx.next
        Some(nx.value.asInstanceOf[V])
      }
      else next.removeAttachment(key)
    }

    def allAttachments: List[Any] =
      if (next == null) Nil else next.allAttachments
  }

  private[Attachment] class Link[+V](val key: Key[V], val value: V, var next: Link[_])
      extends AttachmentLink[V] {
    override def allAttachments: List[Any] = value :: super.allAttachments
  }

  class Container extends AttachmentLink[Null] {
    private[Attachment] def key = NullKey
    private[Attachment] def value: Null = unsupported("value")
    private[Attachment] var next: Link[_] = null
  }
}