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
package sys

import scala.language.`2.13`
import scala.collection.mutable

/** The internal implementation of scala.sys.Prop.
 *
 *  @tparam T the type of the property value after conversion from `String`
 *  @param key the system property key used to look up the value
 *  @param valueFn the function that converts the raw `String` property value to type `T`
 */
private[sys] class PropImpl[+T](val key: String, valueFn: String => T) extends Prop[T] {
  /** Returns the property value converted by `valueFn`, or `zero` if the property is not set. */
  def value: T = if (isSet) valueFn(get) else zero
  /** Indicates whether the underlying map contains `key`. */
  def isSet    = underlying contains key
  /** Sets the property to the given string value.
   *
   *  @param newValue the new string value to store under `key`
   *  @return the previous string value, or `null` if the property was unset
   */
  def set(newValue: String): String | Null = {
    val old: String | Null = if (isSet) get else null
    underlying(key) = newValue
    old
  }
  /** Sets the property to the string form of the given value.
   *
   *  A `null` value is passed on unchanged, which causes a `NullPointerException`
   *  in `java.lang.System.setProperty`.
   *
   *  @tparam T1 a supertype of `T`, used as the input type since `Prop` is covariant in `T`
   *  @param newValue the value whose string form becomes the new property value
   *  @return the previous converted value, or `zero` if the property was unset
   */
  def setValue[T1 >: T](newValue: T1): T = {
    val old = value
    if (newValue == null) set(null.asInstanceOf[String]) // will cause NPE in java.lang.System.setProperty
    else set("" + newValue)
    old
  }
  /** Returns the current string value, or the empty string if the property is not set. */
  def get: String =
    if (isSet) underlying.getOrElse(key, "").nn
    else ""

  /** Removes the property from the underlying map. */
  def clear(): Unit = underlying -= key
  /** Returns `Some` of the converted value if the property is set, `None` otherwise. */
  def option: Option[T] = if (isSet) Some(value) else None
  /** Returns the converted property value if the property is set, otherwise the given alternative.
   *
   *  @tparam T1 a supertype of `T`, the result type
   *  @param alt the alternative value, evaluated only if the property is not set
   */
  def or[T1 >: T](alt: => T1): T1 = if (isSet) value else alt

  /** The underlying property map, in our case always `sys.props`. */
  protected def underlying: mutable.Map[String, String | Null] = scala.sys.props
  /** Returns the value used when the property is unset, obtained by casting `null` to `T`. */
  protected def zero: T = null.asInstanceOf[T]
  private def getString = if (isSet) "currently: " + get else "unset"
  /** Returns the key followed by `(currently: <value>)`, where `<value>` is the current
   *  string value, or by `(unset)` if the property is not set.
   */
  override def toString() = s"$key ($getString)"
}

private[sys] abstract class CreatorImpl[+T](f: String => T) extends Prop.Creator[T] {
  /** Creates a `Prop[T]` for the given key that converts its raw `String` value with `f`.
   *
   *  @param key the property name used for lookup
   *  @return a new `Prop[T]` backed by `key` and `f`
   */
  def apply(key: String): Prop[T] = new PropImpl[T](key, f)
}
