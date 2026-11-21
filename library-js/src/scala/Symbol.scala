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

import language.experimental.captureChecking
import scala.language.`2.13`

import scala.scalajs.js
import scala.scalajs.LinkingInfo._

/** This class provides a simple way to get unique objects for equal strings.
 *  Since symbols are interned, they can be compared using reference equality.
 */
final class Symbol private (val name: String) extends Serializable {
  /** A string representation of this symbol. */
  override def toString(): String = s"Symbol($name)"

  @throws(classOf[java.io.ObjectStreamException])
  private def readResolve(): Any = Symbol.apply(name)
  override def hashCode() = name.hashCode()
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

object Symbol extends UniquenessCache[String, Symbol] {
  override def apply(name: String): Symbol = super.apply(name)
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

// Modified to use Scala.js specific cache
private[scala] abstract class UniquenessCache[K, V] {
  private val cacheJS: js.Dictionary[V] | Null = {
    linkTimeIf(moduleKind != ModuleKind.WasmModule) {
      js.Dictionary.empty[V]
    } {
      null
    }
  }

  private val cacheWasm: java.util.HashMap[String, V] | Null = {
    linkTimeIf(moduleKind == ModuleKind.WasmModule) {
      new java.util.HashMap[String, V]()
    } {
      null
    }
  }

  protected def valueFromKey(k: String): V
  protected def keyFromValue(v: V): Option[String]

  def apply(name: String): V = {
    linkTimeIf(moduleKind == ModuleKind.WasmModule) {
      cacheWasm.nn.computeIfAbsent(name, valueFromKey(_))
    } {
      cacheJS.nn.getOrElseUpdate(name, valueFromKey(name))
    }
  }

  def unapply(other: V): Option[String] = keyFromValue(other)
}
