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
import scala.runtime.ClassValueCompat._

private[scala] abstract class ClassValueCompat[T] extends ClassValueInterface[T] { self =>
  private val instance: ClassValueInterface[T] =
    if (classValueAvailable) new JavaClassValue()
    else new FallbackClassValue()

  private class JavaClassValue extends ClassValue[T] with ClassValueInterface[T] {
    /** Computes the value for `cls` by delegating to the enclosing
     *  `ClassValueCompat`'s `computeValue`; `java.lang.ClassValue` caches
     *  the result per class.
     *
     *  @param cls the class to compute the value for
     *  @return the computed value
     */
    override def computeValue(cls: Class[?]): T = self.computeValue(cls)
  }

  private class FallbackClassValue extends ClassValueInterface[T] {
    /** Returns the value for `cls`, computed by the enclosing
     *  `ClassValueCompat`'s `computeValue` on every call: the fallback
     *  caches nothing.
     *
     *  @param cls the class to compute the value for
     */
    override def get(cls: Class[?]): T = self.computeValue(cls)

    /** Does nothing: the fallback caches no values, so there is nothing to
     *  remove.
     *
     *  @param cls never used
     */
    override def remove(cls: Class[?]): Unit = {}
  }

  /** Returns the value associated with `cls`: when `java.lang.ClassValue`
   *  is available, from its per-class cache, computed by `computeValue` on
   *  first access; otherwise by calling `computeValue` on every call.
   *
   *  @param cls the class to get the value for
   */
  def get(cls: Class[?]): T = instance.get(cls)

  /** Removes the cached value for `cls`, so that the next `get` recomputes
   *  it; does nothing when `java.lang.ClassValue` is unavailable, since
   *  nothing is cached then.
   *
   *  @param cls the class whose cached value to remove
   */
  def remove(cls: Class[?]): Unit = instance.remove(cls)

  /** Computes the value to associate with `cls`.
   *
   *  Called by `get`: when backed by `java.lang.ClassValue`, on the first
   *  access for each class (and again after `remove`); otherwise on every
   *  call.
   *
   *  @param cls the class to compute the value for
   *  @return the value to associate with `cls`
   */
  protected def computeValue(cls: Class[?]): T
}

private[scala] object ClassValueCompat {
  /** A common interface over `java.lang.ClassValue` and the non-caching
   *  fallback used on runtimes where that class cannot be loaded.
   *
   *  @tparam T the type of the value derived from a class
   */
  trait ClassValueInterface[T] {
    /** Returns the value for `cls`, computing it if necessary.
     *
     *  @param cls the class to get the value for
     */
    def get(cls: Class[?]): T

    /** Removes any cached value for `cls`.
     *
     *  @param cls the class whose cached value to remove
     */
    def remove(cls: Class[?]): Unit
  }

  private val classValueAvailable: Boolean = try {
    Class.forName("java.lang.ClassValue", false, classOf[Object].getClassLoader)
    true
  } catch {
    case _: ClassNotFoundException => false
  }
}
