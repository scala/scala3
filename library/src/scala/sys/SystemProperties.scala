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
import scala.collection.{mutable, Iterator}
import scala.jdk.CollectionConverters._
import java.security.AccessControlException
import scala.language.implicitConversions

/** A bidirectional map wrapping the java System properties.
 *  Changes to System properties will be immediately visible in the map,
 *  and modifications made to the map will be immediately applied to the
 *  System properties.  If a security manager is in place which prevents
 *  the properties from being read or written, the AccessControlException
 *  will be caught and discarded.
 *  @define Coll `collection.mutable.Map`
 *  @define coll mutable map
 */
class SystemProperties
extends mutable.AbstractMap[String, String | Null] {

  /** Returns a new, empty $coll which is not backed by the System properties. */
  override def empty: mutable.Map[String, String | Null] = mutable.Map[String, String | Null]()
  /** Returns `null`, the default value of this $coll, which `apply` returns
   *  for a key with no value in the map.
   *
   *  @param key the name of the property being looked up, which is not used
   */
  override def default(key: String): String | Null = null

  /** Returns an iterator over the string-valued System properties, that is,
   *  those whose name and value are both `String`s, or an empty iterator if
   *  the properties may not be read.
   */
  def iterator: Iterator[(String, String)] = wrapAccess {
    val ps = System.getProperties()
    names map (k => (k, ps.getProperty(k))) filter (_._2 ne null)
  } getOrElse Iterator.empty

  /** Returns `true` if this $coll exposes no properties, which is the case
   *  when no string-valued System property is set, or if the properties may
   *  not be read.
   */
  override def isEmpty: Boolean = iterator.isEmpty
  /** Returns an iterator over the names of the string-valued System properties,
   *  that is, those whose name and value are both `String`s, or an empty
   *  iterator if the properties may not be read.
   */
  def names: Iterator[String] = wrapAccess (
    System.getProperties().stringPropertyNames().asScala.iterator
  ) getOrElse Iterator.empty

  /** Optionally returns the value of the System property named `key`.
   *
   *  @param key the name of the property to look up
   *  @return the property value, or `None` if the property is unset, has a
   *          non-`String` value, or may not be read
   */
  def get(key: String): Option[String] =
    wrapAccess(Option(System.getProperty(key))) flatMap (x => x)
  /** Returns `true` if a System property named `key` is set to a `String` value.
   *
   *  @param key the name of the property to look up
   *  @return whether the property is set to a `String` value, or `false` if the
   *          properties may not be read
   */
  override def contains(key: String): Boolean =
    wrapAccess(super.contains(key)) exists (x => x)

  /** Removes all of the System properties, doing nothing if they may not be written. */
  override def clear(): Unit = wrapAccess(System.getProperties().clear())
  /** Removes the System property named `key`, doing nothing if it may not be written.
   *
   *  @param key the name of the property to remove
   *  @return this $coll
   */
  def subtractOne (key: String): this.type = { wrapAccess(System.clearProperty(key)) ; this }
  /** Sets the System property named by the first element of `kv` to its second
   *  element, doing nothing if the property may not be written.
   *
   *  @param kv the name and value of the property to set
   *  @return this $coll
   *  @throws NullPointerException if the name or the value is `null`
   */
  def addOne (kv: (String, String | Null)): this.type = { wrapAccess(System.setProperty(kv._1, kv._2)) ; this }

  /** Evaluates `body`, catching and discarding any `AccessControlException` it
   *  raises.  This is intended for accessing the System properties, where such
   *  an exception indicates that a security manager denied access.
   *
   *  @tparam T the return type of the body expression
   *  @param body the code to evaluate, typically a System property access
   *  @return the result of `body` wrapped in `Some`, or `None` if an
   *          `AccessControlException` was caught
   */
  @annotation.nowarn("cat=deprecation") // AccessControlException is deprecated on JDK 17
  def wrapAccess[T](body: => T): Option[T] =
    try Some(body) catch { case _: AccessControlException => None }
}

/** The values in SystemProperties can be used to access and manipulate
 *  designated system properties.  See `scala.sys.Prop` for particulars.
 *  @example ```
 *    if (!headless.isSet) headless.enable()
 *  ```
 */
object SystemProperties {
  /** An unenforceable, advisory only place to do some synchronization when
   *  mutating system properties.
   *
   *  @tparam T the return type of the body expression
   *  @param body the code to execute while holding the lock
   *  @return the result of evaluating `body` while holding the lock
   */
  def exclusively[T](body: => T): T = this.synchronized:
    body

  /** Returns this companion object, so that the properties it defines can be
   *  selected on any `SystemProperties` instance.
   *
   *  @param p the instance being converted, whose value is not used
   */
  implicit def systemPropertiesToCompanion(p: SystemProperties): SystemProperties.type = this

  private final val HeadlessKey            = "java.awt.headless"
  private final val PreferIPv4StackKey     = "java.net.preferIPv4Stack"
  private final val PreferIPv6AddressesKey = "java.net.preferIPv6Addresses"
  private final val NoTraceSuppressionKey  = "scala.control.noTraceSuppression"

  /** Returns a short description of the meaning of the System property named `key`.
   *
   *  @param key the name of the property to describe
   *  @return the description, or the empty string if `key` is not one of the
   *          properties defined here
   */
  def help(key: String): String = key match {
    case HeadlessKey            => "system should not utilize a display device"
    case PreferIPv4StackKey     => "system should prefer IPv4 sockets"
    case PreferIPv6AddressesKey => "system should prefer IPv6 addresses"
    case NoTraceSuppressionKey  => "scala should not suppress any stack trace creation"
    case _                      => ""
  }

  /** The `java.awt.headless` property, indicating that the system should not use a
   *  display device.  True when the key is set to the empty string or, compared
   *  case-insensitively, `"true"`.
   */
  lazy val headless: BooleanProp            = BooleanProp.keyExists(HeadlessKey)
  /** The `java.net.preferIPv4Stack` property, indicating that the system should
   *  prefer IPv4 sockets.  True when the key is set to the empty string or, compared
   *  case-insensitively, `"true"`.
   */
  lazy val preferIPv4Stack: BooleanProp     = BooleanProp.keyExists(PreferIPv4StackKey)
  /** The `java.net.preferIPv6Addresses` property, indicating that the system should
   *  prefer IPv6 addresses.  True when the key is set to the empty string or, compared
   *  case-insensitively, `"true"`.
   */
  lazy val preferIPv6Addresses: BooleanProp = BooleanProp.keyExists(PreferIPv6AddressesKey)
  /** The `scala.control.noTraceSuppression` property, indicating that Scala should not
   *  suppress any stack trace creation.  True only when the key is set to `"true"`,
   *  compared case-insensitively.
   */
  lazy val noTraceSuppression: BooleanProp  = BooleanProp.valueIsTrue(NoTraceSuppressionKey)
}
