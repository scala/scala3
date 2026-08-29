/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.language.`2.13`

/** This class provides a simple way to get unique objects for equal strings.
 *  Since symbols are interned, they can be compared using reference equality.
 */
final class Symbol private (val name: String) extends Serializable {
  /** A string representation of this symbol. */
  override def toString(): String = s"Symbol($name)"

  @throws(classOf[java.io.ObjectStreamException])
  private def readResolve(): Any = Symbol.apply(name)
  /** Returns the hash code of this symbol's name. Throws a `NullPointerException`
   *  if this symbol was created with a `null` name.
   */
  override def hashCode() = name.hashCode()
  /** Tests whether `other` is this very symbol. Because symbols are interned,
   *  reference equality coincides with equality of names.
   *
   *  @param other the value to compare with this symbol
   */
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

object Symbol extends UniquenessCache[String, Symbol] {
  /** Returns the unique symbol with the given name, creating and caching it if
   *  no such symbol exists yet.
   *
   *  @param name the name of the symbol
   */
  override def apply(name: String): Symbol = super.apply(name)
  /** Constructs a fresh symbol with the given name, without consulting the cache.
   *
   *  @param name the name of the symbol to create
   *  @return a newly allocated `Symbol` for the cache to intern
   */
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  /** Returns the cache key under which `sym` is interned, namely its name.
   *
   *  @param sym the symbol to take the key from
   *  @return the symbol's name, always wrapped in `Some`
   */
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

/** This is private so it won't appear in the library API, but
 *  abstracted to offer some hope of reusability.  
 *
 *  @tparam K the type of keys used for cache lookup, held via weak references
 *  @tparam V the type of cached values, weakly referenced and constructed from keys via `valueFromKey`
 */
private[scala] abstract class UniquenessCache[K, V] {
  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V | Null = {
      rlock.lock
      try {
        val reference = map.get(name)
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
          map.remove(name)
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }
    cached() match {
      case null => updateCache()
      case res  => res
    }
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
