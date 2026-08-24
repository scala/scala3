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
package runtime

import scala.language.`2.13`
import java.lang.reflect.{ Method => JMethod }
import java.lang.{ Class => JClass }

import scala.annotation.tailrec

/** An element of a polymorphic object cache.
 *  This class is referred to by the `CleanUp` phase. Each `PolyMethodCache` chain
 *  must only relate to one method as `PolyMethodCache` does not identify
 *  the method name and argument types. In practice, one variable will be
 *  generated per call point, and will uniquely relate to the method called
 *  at that point, making the method name and argument types irrelevant. 
 */
/* TODO: if performance is acceptable, PolyMethodCache should be made generic on the method type */
private[scala] sealed abstract class MethodCache {
  /** Searches for a cached method in the `MethodCache` chain that
   *  is compatible with receiver class `forReceiver`. If none is cached,
   *  `null` is returned. If `null` is returned, find's caller should look-
   *  up the right method using whichever means it prefers, and add it to
   *  the cache for later use. 
   *
   *  @param forReceiver the runtime `Class` of the receiver object to look up in the cache
   *  @return the cached `JMethod` compatible with `forReceiver`, or `null` if no such method is cached (in which case the caller should look up the method and `add` it)
   */
  def find(forReceiver: JClass[?]): JMethod | Null
  /** Returns a cache that also resolves `forMethod` for receiver class
   *  `forReceiver`, to replace this cache at the call point. This cache
   *  itself is not mutated: implementations return either a new cache
   *  extending this one or this cache unchanged.
   *
   *  @param forReceiver the runtime `Class` of the receiver object the method was looked up for
   *  @param forMethod the method resolved for `forReceiver`
   *  @return the cache to use for subsequent look-ups at this call point
   */
  def add(forReceiver: JClass[?], forMethod: JMethod): MethodCache
}

private[scala] final class EmptyMethodCache extends MethodCache {

  /** Returns `null`: the empty cache contains no methods, so the caller
   *  should look up the method and `add` it.
   *
   *  @param forReceiver the runtime `Class` of the receiver object; never used
   */
  def find(forReceiver: JClass[?]): JMethod | Null = null

  /** Returns a new single-entry [[PolyMethodCache]] of complexity 1 that
   *  resolves `forMethod` for receiver class `forReceiver`, with this empty
   *  cache as the end of its chain.
   *
   *  @param forReceiver the runtime `Class` of the receiver object the method was looked up for
   *  @param forMethod the method resolved for `forReceiver`
   *  @return the new one-entry cache
   */
  def add(forReceiver: JClass[?], forMethod: JMethod): MethodCache =
    new PolyMethodCache(this, forReceiver, forMethod, 1)

}

private[scala] final class MegaMethodCache(
  private val forName: String,
  private val forParameterTypes: Array[JClass[?]]
) extends MethodCache {

  /** Returns the public method of `forReceiver` with this cache's method
   *  name and parameter types, resolved reflectively via `getMethod` on
   *  every call: a mega-morphic cache stores no per-receiver entries.
   *
   *  @param forReceiver the runtime `Class` of the receiver object to resolve the method on
   *  @return the resolved method; never `null`
   *  @throws NoSuchMethodException if `forReceiver` has no public method
   *          with this cache's name and parameter types
   */
  def find(forReceiver: JClass[?]): JMethod | Null =
    forReceiver.getMethod(forName, forParameterTypes*)

  /** Returns this cache unchanged: a mega-morphic cache resolves methods
   *  reflectively in `find` and records no per-receiver entries.
   *
   *  @param forReceiver never used
   *  @param forMethod never used
   */
  def add(forReceiver: JClass[?], forMethod: JMethod): MethodCache = this

}

private[scala] final class PolyMethodCache(
  private val next: MethodCache,
  private val receiver: JClass[?],
  private val method: JMethod,
  private val complexity: Int
) extends MethodCache {

  /** To achieve tail recursion this must be a separate method
   *  from `find`, because the type of next is not `PolyMethodCache`.
   *
   *  @param forReceiver the runtime `Class` of the receiver object to look up in the cache chain via tail recursion
   *  @return the cached `JMethod` whose receiver class is reference-equal to `forReceiver` anywhere in this chain, or `null` if no match exists
   */
  @tailrec private def findInternal(forReceiver: JClass[?]): JMethod | Null =
    if (forReceiver eq receiver) method
    else next match {
      case x: PolyMethodCache => x findInternal forReceiver
      case _                  => next find forReceiver
    }

  /** Returns the cached method whose receiver class is reference-equal to
   *  `forReceiver` anywhere in this chain, or `null` if no entry matches
   *  (in which case the caller should look up the method and `add` it).
   *
   *  @param forReceiver the runtime `Class` of the receiver object to look up in the cache chain
   */
  def find(forReceiver: JClass[?]): JMethod | Null = findInternal(forReceiver)

  // TODO: come up with a more realistic number
  final private val MaxComplexity = 160

  /** Returns a new [[PolyMethodCache]] that prepends the entry
   *  (`forReceiver`, `forMethod`) to this chain, unless this chain has
   *  reached `MaxComplexity` entries, in which case the call point has
   *  turned mega-morphic: returns a [[MegaMethodCache]] for the method's
   *  name and parameter types, discarding the per-receiver entries.
   *
   *  @param forReceiver the runtime `Class` of the receiver object the method was looked up for
   *  @param forMethod the method resolved for `forReceiver`
   *  @return the extended chain, or a mega-morphic cache at the cutover
   */
  def add(forReceiver: JClass[?], forMethod: JMethod): MethodCache =
    if (complexity < MaxComplexity)
      new PolyMethodCache(this, forReceiver, forMethod, complexity + 1)
    else
      new MegaMethodCache(forMethod.getName, forMethod.getParameterTypes)
}
