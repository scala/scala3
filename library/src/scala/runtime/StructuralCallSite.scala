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

package scala.runtime

import scala.language.`2.13`
import java.lang.invoke._
import java.lang.ref.SoftReference
import java.lang.reflect.Method

/** The state of one structural (reflective) call site: the parameter types
 *  of the call and a softly referenced [[MethodCache]] mapping receiver
 *  classes to resolved methods.
 *
 *  Created by `StructuralCallSite.bootstrap`, which the JVM invokes via
 *  `invokedynamic` for each structural call site the compiler emits.
 */
final class StructuralCallSite private (callType: MethodType) {
  private var cache: SoftReference[MethodCache] =  new SoftReference(new EmptyMethodCache)

  /** The parameter classes of the called method, extracted from the call
   *  site's `MethodType`.
   */
  val parameterTypes: Array[Class[?]] = callType.parameterArray

  /** Returns the current method cache, installing and returning a fresh
   *  [[EmptyMethodCache]] if the previous cache's soft reference was
   *  cleared by the garbage collector.
   */
  def get: MethodCache = {
    var cache = this.cache.get
    if (cache == null) {
      cache = new EmptyMethodCache
      this.cache = new SoftReference(cache)
    }
    cache
  }

  /** Returns the method this call site resolved for receiver class
   *  `receiver`, or `null` if none is cached, in which case the caller
   *  should look up the method and `add` it. Once the cache has turned
   *  mega-morphic, resolves the method reflectively instead of consulting
   *  per-receiver entries; see [[MethodCache.find]].
   *
   *  @param receiver the runtime `Class` of the receiver object
   *  @throws NoSuchMethodException from the reflective lookup a mega-morphic cache
   *          performs, in place of returning `null`
   */
  def find(receiver: Class[?]): Method | Null = get.find(receiver)

  /** Records that `m` is the method to call for receiver class `receiver`,
   *  replacing this call site's cache with the extended one, and returns `m`.
   *
   *  Once the cache has turned mega-morphic it records nothing per receiver and
   *  returns itself, so the call site's cache is then left as it was.
   *
   *  @param receiver the runtime `Class` of the receiver object
   *  @param m the method resolved for `receiver`
   *  @return `m`
   */
  def add(receiver: Class[?], m: Method): Method = {
    cache = new SoftReference(get.add(receiver, m))
    m
  }
}

object StructuralCallSite {
  /** Bootstrap method that the JVM invokes, via the `invokedynamic`
   *  instruction the compiler emits for a structural call, to link the call
   *  site.
   *
   *  @param lookup never used
   *  @param invokedName never used
   *  @param invokedType never used
   *  @param reflectiveCallType the `MethodType` of the structural call
   *  @return a `ConstantCallSite` whose target constantly returns the one
   *          `StructuralCallSite` created here for `reflectiveCallType`
   */
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, reflectiveCallType: MethodType): CallSite = {
    val structuralCallSite = new StructuralCallSite(reflectiveCallType)
    new ConstantCallSite(MethodHandles.constant(classOf[StructuralCallSite], structuralCallSite))
  }
}
