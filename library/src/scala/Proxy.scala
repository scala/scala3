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

/** This class implements a simple proxy that forwards all calls to
 *  the public, non-final methods defined in class `Any` to another
 *  object self.  Those methods are:
 *  ```
 *    def hashCode(): Int
 *    def equals(other: Any): Boolean
 *    def toString(): String
 *  ```
 *  **Note:** forwarding methods in this way will most likely create
 *  an asymmetric equals method, which is not generally recommended.
 */
@deprecated("Explicitly override hashCode, equals and toString instead.", "2.13.0")
trait Proxy extends Any {
  /** The object to which the `Any` methods of this proxy are forwarded. */
  def self: Any

  /** Returns the hash code of `self`. */
  override def hashCode(): Int = self.hashCode
  /** Tests whether `that` is equal to `self`.
   *
   *  @param that the object to compare with `self`
   *  @return `true` if `that` is a reference to this proxy or to `self`, or if
   *          `that` equals `self`; `false` otherwise
   */
  override def equals(that: Any): Boolean = that match {
    case null  => false
    case _     =>
      val x = that.asInstanceOf[AnyRef]
      (x eq this.asInstanceOf[AnyRef]) || (x eq self.asInstanceOf[AnyRef]) || (x.equals(self))
  }
  /** Returns the string representation of `self`. */
  override def toString() = "" + self
}

@deprecated("All members of this object are deprecated.", "2.13.0")
object Proxy {
  /** A proxy which exposes the type it is proxying for via a type parameter.
   */
  @deprecated("Explicitly override hashCode, equals and toString instead.", "2.13.0")
  trait Typed[T] extends Any with Proxy {
    /** The object to which the `Any` methods of this proxy are forwarded,
     *  narrowed to the proxied type `T`.
     */
    def self: T
  }
}
