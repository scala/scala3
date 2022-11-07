package dotty.tools.dotc.util

/** Defines a key type with which to tag properties, such as attachments
 *  or context properties
 */
object Property {

  /** The class of keys for properties of type V */
  class Key[+V]

  /**
   * The class of keys for sticky properties of type V
   *
   * Sticky properties are properties that should be copied over when their container
   * is copied.
   */
  class StickyKey[+V] extends Key[V]
}
