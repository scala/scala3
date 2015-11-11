/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools.dotc.util

/** A common class for lightweight sets.
 */
abstract class Set[T >: Null] {

  def findEntry(x: T): T

  def addEntry(x: T): Unit

  def iterator: Iterator[T]

  def contains(x: T): Boolean =
    findEntry(x) != null

  def clear: Unit
}
