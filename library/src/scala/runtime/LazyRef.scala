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

/** Classes used as holders for lazy vals defined in methods. */

@SerialVersionUID(1L)
class LazyRef[T] extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: T = compiletime.uninitialized
  def value: T = _value
  def initialize(value: T): T = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyRef ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyBoolean extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Boolean = compiletime.uninitialized
  def value: Boolean = _value
  def initialize(value: Boolean): Boolean = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyBoolean ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyByte extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Byte = compiletime.uninitialized

  def value: Byte = _value

  def initialize(value: Byte): Byte = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyByte ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyChar extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Char = compiletime.uninitialized
  def value: Char = _value
  def initialize(value: Char): Char = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyChar ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyShort extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Short = compiletime.uninitialized
  def value: Short = _value
  def initialize(value: Short): Short = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyShort ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyInt extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Int = compiletime.uninitialized
  def value: Int = _value
  def initialize(value: Int): Int = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyInt ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyLong extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Long = compiletime.uninitialized
  def value: Long = _value
  def initialize(value: Long): Long = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyLong ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyFloat extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Float = compiletime.uninitialized
  def value: Float = _value
  def initialize(value: Float): Float = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyFloat ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyDouble extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  private var _value: Double = compiletime.uninitialized
  def value: Double = _value
  def initialize(value: Double): Double = {
    _value = value
    _initialized = true
    value
  }

  override def toString() = s"LazyDouble ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

@SerialVersionUID(1L)
class LazyUnit extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  def initialized = _initialized

  def initialize(): Unit = _initialized = true

  override def toString() = s"LazyUnit${if (_initialized) "" else " thunk"}"
}
