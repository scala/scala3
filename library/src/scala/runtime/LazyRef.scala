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
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: T = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `null` if no value has
   *  been stored yet: check `initialized` before reading.
   */
  def value: T = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: T): T = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyRef of: "` followed by the value if initialized, `"LazyRef thunk"` otherwise. */
  override def toString() = s"LazyRef ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Boolean` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyBoolean extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Boolean = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `false` if no value has
   *  been stored yet: check `initialized` before reading.
   */
  def value: Boolean = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Boolean): Boolean = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyBoolean of: "` followed by the value if initialized, `"LazyBoolean thunk"` otherwise. */
  override def toString() = s"LazyBoolean ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Byte` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyByte extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Byte = compiletime.uninitialized

  /** Returns the value stored by `initialize`, or `0` if no value has been
   *  stored yet: check `initialized` before reading.
   */
  def value: Byte = _value

  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Byte): Byte = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyByte of: "` followed by the value if initialized, `"LazyByte thunk"` otherwise. */
  override def toString() = s"LazyByte ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Char` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyChar extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Char = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or the null character if no
   *  value has been stored yet: check `initialized` before reading.
   */
  def value: Char = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Char): Char = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyChar of: "` followed by the value if initialized, `"LazyChar thunk"` otherwise. */
  override def toString() = s"LazyChar ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Short` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyShort extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Short = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `0` if no value has been
   *  stored yet: check `initialized` before reading.
   */
  def value: Short = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Short): Short = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyShort of: "` followed by the value if initialized, `"LazyShort thunk"` otherwise. */
  override def toString() = s"LazyShort ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Int` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyInt extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Int = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `0` if no value has been
   *  stored yet: check `initialized` before reading.
   */
  def value: Int = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Int): Int = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyInt of: "` followed by the value if initialized, `"LazyInt thunk"` otherwise. */
  override def toString() = s"LazyInt ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Long` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyLong extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Long = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `0` if no value has been
   *  stored yet: check `initialized` before reading.
   */
  def value: Long = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Long): Long = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyLong of: "` followed by the value if initialized, `"LazyLong thunk"` otherwise. */
  override def toString() = s"LazyLong ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Float` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyFloat extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Float = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `0.0` if no value has
   *  been stored yet: check `initialized` before reading.
   */
  def value: Float = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Float): Float = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyFloat of: "` followed by the value if initialized, `"LazyFloat thunk"` otherwise. */
  override def toString() = s"LazyFloat ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Double` defined in a method.
 *
 *  Like [[LazyRef]], but specialized to avoid boxing: stores the value and
 *  an initialized flag, set once by `initialize` under the
 *  compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyDouble extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once a value has been stored by `initialize`.
   *
   *  The flag is volatile: after a read of `true`, a subsequent read of
   *  `value` sees the value stored before the flag was set.
   */
  def initialized = _initialized

  private var _value: Double = compiletime.uninitialized
  /** Returns the value stored by `initialize`, or `0.0` if no value has
   *  been stored yet: check `initialized` before reading.
   */
  def value: Double = _value
  /** Stores `value` and then sets the initialized flag, in that order, so
   *  that a reader that observes `initialized` as `true` also observes the
   *  stored value.
   *
   *  Performs no locking and does not check the flag: racing or repeated
   *  calls each overwrite the stored value. The compiler-generated caller
   *  ensures a single initialization by synchronizing on this holder.
   *
   *  @param value the computed value of the lazy val
   *  @return `value`
   */
  def initialize(value: Double): Double = {
    _value = value
    _initialized = true
    value
  }

  /** Returns `"LazyDouble of: "` followed by the value if initialized, `"LazyDouble thunk"` otherwise. */
  override def toString() = s"LazyDouble ${if (_initialized) s"of: ${_value}" else "thunk"}"
}

/** A holder for a lazy val of type `Unit` defined in a method.
 *
 *  Like [[LazyRef]], but with no value to store: records only whether the
 *  right-hand side has been evaluated, via a flag set once by `initialize`
 *  under the compiler-generated caller's synchronization.
 */
@SerialVersionUID(1L)
class LazyUnit extends Serializable {
  @volatile private var _initialized: Boolean = compiletime.uninitialized
  /** Returns `true` once `initialize` has been called, that is, once the
   *  right-hand side of the lazy val has been evaluated for its side
   *  effects. The flag is volatile.
   */
  def initialized = _initialized

  /** Sets the initialized flag, recording that the right-hand side of the
   *  lazy val has been evaluated. Performs no locking; the
   *  compiler-generated caller synchronizes on this holder.
   */
  def initialize(): Unit = _initialized = true

  /** Returns `"LazyUnit"` if initialized, `"LazyUnit thunk"` otherwise. */
  override def toString() = s"LazyUnit${if (_initialized) "" else " thunk"}"
}
