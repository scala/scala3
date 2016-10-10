package dotty.tools.dotc.util

/** Defines a key type with which to tag properties, such as attachments
 *  or context properties
 */
object Property {

  /** The class of keys for properties of type V */
  class Key[+V]
}