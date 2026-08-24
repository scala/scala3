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
import scala.language.implicitConversions

/** A few additional conveniences for Boolean properties. */
trait BooleanProp extends Prop[Boolean] {
  /** The semantics of value are determined at Prop creation.  See methods
   *  `valueIsTrue` and `keyExists` in object BooleanProp for examples.
   *
   *  @return   true if the current String is considered true, false otherwise
   */
  def value: Boolean

  /** Alter this property so that `value` will be true. */
  def enable(): Unit

  /** Alter this property so that `value` will be false. */
  def disable(): Unit

  /** Toggle the property between enabled and disabled states. */
  def toggle(): Unit
}

object BooleanProp {
  private[sys]
  /** A `BooleanProp` backed by a system property, whose truth is decided by `valueFn`.
   *
   *  @param key the system property key used to look up the value
   *  @param valueFn the function deciding whether the raw `String` property value counts as true
   */
  class BooleanPropImpl(key: String, valueFn: String => Boolean) extends PropImpl(key, valueFn) with BooleanProp {
    /** Sets the property to `newValue`, clearing it instead when `newValue` is `false`.
     *
     *  @tparam T1 a supertype of `Boolean`, used as the input type since `Prop` is covariant in `T`
     *  @param newValue the value to set for this property
     *  @return the previous value of this property
     */
    override def setValue[T1 >: Boolean](newValue: T1): Boolean = newValue match {
      case x: Boolean if !x   => val old = value ; clear() ; old
      case x                  => super.setValue(newValue)
    }
    /** Sets the raw property value to `"true"`, so that `value` will be whatever `valueFn` returns for `"true"`. */
    def enable()  = this setValue true
    /** Removes the property from the underlying map, so that `value` will be false. */
    def disable() = this.clear()
    /** Disables the property if `value` is currently true, otherwise enables it. */
    def toggle()  = if (value) disable() else enable()
  }
  private[sys]
  /** A `BooleanProp` with a fixed value, backed by no map, whose mutating operations do nothing.
   *
   *  @param key the name of the property
   *  @param value the constant value of this property
   */
  class ConstantImpl(val key: String, val value: Boolean) extends BooleanProp {
    /** Equal to `value`: a constant-true property counts as set, a constant-false one as unset. */
    val isSet = value
    /** Ignores `newValue` and returns the string form of the constant value.
     *
     *  @param newValue the new string value, which is discarded
     */
    def set(newValue: String) = "" + value
    /** Ignores `newValue` and leaves the constant value in place.
     *
     *  @tparam T1 a supertype of `Boolean`, used as the input type since `Prop` is covariant in `T`
     *  @param newValue the value to set for this property, which is discarded
     *  @return the constant value of this property
     */
    def setValue[T1 >: Boolean](newValue: T1): Boolean = value
    /** Returns the string form of the constant value, either `"true"` or `"false"`. */
    def get: String = "" + value
    /** Returns `Some(true)` if the constant value is true, `None` otherwise. */
    def option = if (isSet) Some(value) else None
    //def or[T1 >: Boolean](alt: => T1): T1 = if (value) true else alt

    /** Does nothing, since the value of this property is constant. */
    def clear() = ()
    /** Does nothing, since the value of this property is constant. */
    def enable() = ()
    /** Does nothing, since the value of this property is constant. */
    def disable() = ()
    /** Does nothing, since the value of this property is constant. */
    def toggle() = ()

    /** The default `false` required by the `Prop` contract, never consulted here
     *  because `value` is fixed at construction.
     */
    protected def zero = false
  }

  /** The java definition of property truth is that the key be in the map and
   *  the value be equal to the String "true", case insensitively.  This method
   *  creates a BooleanProp instance which adheres to that definition.
   *
   *  @tparam T unused type parameter, retained for API compatibility
   *  @param key the name of the system property to look up
   *  @return   A BooleanProp which acts like java's Boolean.getBoolean
   */
  def valueIsTrue[T](key: String): BooleanProp = new BooleanPropImpl(key, _.toLowerCase == "true")

  /** As an alternative, this method creates a BooleanProp which is true
   *  if the key exists in the map and is not assigned a value other than "true",
   *  compared case-insensitively, or the empty string.  This way -Dmy.property
   *  results in a true-valued property, but -Dmy.property=false does not.
   *
   *  @tparam T unused type parameter, retained for API compatibility
   *  @param key the name of the system property to look up
   *  @return   A BooleanProp with a liberal truth policy
   */
  def keyExists[T](key: String): BooleanProp = new BooleanPropImpl(key, s => s == "" || s.equalsIgnoreCase("true"))

  /** A constant true or false property which ignores all method calls.
   *
   *  @param key the name of the system property
   *  @param isOn whether the constant property is true or false
   *  @return a BooleanProp whose value is fixed and whose mutating operations are no-ops
   */
  def constant(key: String, isOn: Boolean): BooleanProp = new ConstantImpl(key, isOn)

  /** Returns the `value` of the given property, so that a `BooleanProp` can be used
   *  where a `Boolean` is expected.
   *
   *  @param b the property to convert
   */
  implicit def booleanPropAsBoolean(b: BooleanProp): Boolean = b.value
}
