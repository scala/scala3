/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.scalajs.js

/** This class provides a simple way to get unique objects for equal strings.
 *  Since symbols are interned, they can be compared using reference equality.
 *  Instances of `Symbol` can be created easily with Scala's built-in quote
 *  mechanism.
 *
 *  For instance, the Scala term `'mysym` will
 *  invoke the constructor of the `Symbol` class in the following way:
 *  `Symbol("mysym")`.
 *
 *  @author  Martin Odersky, Iulian Dragos
 *  @since   1.7
 */
final class Symbol private (val name: String) extends Serializable {
  /** Converts this symbol to a string.
   */
  override def toString(): String = "Symbol(" + name + ")"

  @throws(classOf[java.io.ObjectStreamException])
  private def readResolve(): Any = Symbol.apply(name)
  override def hashCode = name.hashCode()
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

// Modified to use Scala.js specific cache
object Symbol extends JSUniquenessCache[Symbol] {
  override def apply(name: String): Symbol = super.apply(name)
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

private[scala] abstract class JSUniquenessCache[V]
{
  private val cache = js.Dictionary.empty[V]

  protected def valueFromKey(k: String): V
  protected def keyFromValue(v: V): Option[String]

  def apply(name: String): V =
    cache.getOrElseUpdate(name, valueFromKey(name))

  def unapply(other: V): Option[String] = keyFromValue(other)
}

/** This is private so it won't appear in the library API, but
  * abstracted to offer some hope of reusability.  */
/* DELETED for Scala.js
private[scala] abstract class UniquenessCache[K >: js.String, V >: Null]
{

  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private[this] val rwl = new ReentrantReadWriteLock()
  private[this] val rlock = rwl.readLock
  private[this] val wlock = rwl.writeLock
  private[this] val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock
      try {
        val reference = map get name
        if (reference == null) null
        else reference.get  // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          // If we don't remove the old String key from the map, we can
          // wind up with one String as the key and a different String as
          // the name field in the Symbol, which can lead to surprising GC
          // behavior and duplicate Symbols. See scala/bug#6706.
          map remove name
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
*/
