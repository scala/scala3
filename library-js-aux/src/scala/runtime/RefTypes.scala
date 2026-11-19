/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime

import java.io.Serializable

@inline
class BooleanRef(var elem: Boolean) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object BooleanRef {
  def create(elem: Boolean): BooleanRef = new BooleanRef(elem)
  def zero(): BooleanRef = new BooleanRef(false)
}

@inline
class VolatileBooleanRef(var elem: Boolean) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileBooleanRef {
  def create(elem: Boolean): VolatileBooleanRef = new VolatileBooleanRef(elem)
  def zero(): VolatileBooleanRef = new VolatileBooleanRef(false)
}

@inline
class CharRef(var elem: Char) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object CharRef {
  def create(elem: Char): CharRef = new CharRef(elem)
  def zero(): CharRef = new CharRef(0.toChar)
}

@inline
class VolatileCharRef(var elem: Char) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileCharRef {
  def create(elem: Char): VolatileCharRef = new VolatileCharRef(elem)
  def zero(): VolatileCharRef = new VolatileCharRef(0.toChar)
}

@inline
class ByteRef(var elem: Byte) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object ByteRef {
  def create(elem: Byte): ByteRef = new ByteRef(elem)
  def zero(): ByteRef = new ByteRef(0)
}

@inline
class VolatileByteRef(var elem: Byte) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileByteRef {
  def create(elem: Byte): VolatileByteRef = new VolatileByteRef(elem)
  def zero(): VolatileByteRef = new VolatileByteRef(0)
}

@inline
class ShortRef(var elem: Short) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object ShortRef {
  def create(elem: Short): ShortRef = new ShortRef(elem)
  def zero(): ShortRef = new ShortRef(0)
}

@inline
class VolatileShortRef(var elem: Short) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileShortRef {
  def create(elem: Short): VolatileShortRef = new VolatileShortRef(elem)
  def zero(): VolatileShortRef = new VolatileShortRef(0)
}

@inline
class IntRef(var elem: Int) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object IntRef {
  def create(elem: Int): IntRef = new IntRef(elem)
  def zero(): IntRef = new IntRef(0)
}

@inline
class VolatileIntRef(var elem: Int) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileIntRef {
  def create(elem: Int): VolatileIntRef = new VolatileIntRef(elem)
  def zero(): VolatileIntRef = new VolatileIntRef(0)
}

@inline
class LongRef(var elem: Long) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object LongRef {
  def create(elem: Long): LongRef = new LongRef(elem)
  def zero(): LongRef = new LongRef(0)
}

@inline
class VolatileLongRef(var elem: Long) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileLongRef {
  def create(elem: Long): VolatileLongRef = new VolatileLongRef(elem)
  def zero(): VolatileLongRef = new VolatileLongRef(0)
}

@inline
class FloatRef(var elem: Float) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object FloatRef {
  def create(elem: Float): FloatRef = new FloatRef(elem)
  def zero(): FloatRef = new FloatRef(0)
}

@inline
class VolatileFloatRef(var elem: Float) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileFloatRef {
  def create(elem: Float): VolatileFloatRef = new VolatileFloatRef(elem)
  def zero(): VolatileFloatRef = new VolatileFloatRef(0)
}

@inline
class DoubleRef(var elem: Double) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object DoubleRef {
  def create(elem: Double): DoubleRef = new DoubleRef(elem)
  def zero(): DoubleRef = new DoubleRef(0)
}

@inline
class VolatileDoubleRef(var elem: Double) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileDoubleRef {
  def create(elem: Double): VolatileDoubleRef = new VolatileDoubleRef(elem)
  def zero(): VolatileDoubleRef = new VolatileDoubleRef(0)
}

@inline
class ObjectRef[A](var elem: A) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object ObjectRef {
  def create[A](elem: A): ObjectRef[A] = new ObjectRef(elem)
  def zero(): ObjectRef[Object] = new ObjectRef(null.asInstanceOf[Object])
}

@inline
class VolatileObjectRef[A](var elem: A) extends Serializable {
  override def toString(): String = String.valueOf(elem)
}

object VolatileObjectRef {
  def create[A](elem: A): VolatileObjectRef[A] = new VolatileObjectRef(elem)
  def zero(): VolatileObjectRef[Object] = new VolatileObjectRef(null.asInstanceOf[Object])
}
