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

import scala.language.`2.13`

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

object Symbol extends UniquenessCache[String, Symbol] {
  override def apply(name: String): Symbol = super.apply(name)
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

// Modified to use Scala.js specific cache
private[scala] abstract class UniquenessCache[K, V] {
  private val cache = js.Dictionary.empty[V]

  protected def valueFromKey(k: String): V
  protected def keyFromValue(v: V): Option[String]

  def apply(name: String): V =
    cache.getOrElseUpdate(name, valueFromKey(name))

  def unapply(other: V): Option[String] = keyFromValue(other)
}