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

package org.scalajs.testsuite.wasminterop

import scala.annotation.nowarn

import scala.scalajs.wasm
import scala.scalajs.wasm.annotation._

object utils {
  @noinline
  def assertEquals(expected: Any, actual: Any): Unit = {
    if (actual != expected)
      throw new AssertionError(s"expected $expected; but got $actual")
  }
}

object foo {
  @WasmImport("foo", "i32Times11")
  def i32Times11(x: Int): Int = wasm.native

  @WasmImport("foo", "i64Times11")
  def i64Times11(x: Long): Long = wasm.native

  @WasmImport("foo", "f32Times11dot5")
  def f32Times11dot5(x: Float): Float = wasm.native

  @WasmImport("foo", "f64Times11dot5")
  def f64Times11dot5(x: Double): Double = wasm.native

  @WasmImport("foo", "getFooMarker")
  def getFooMarker(): Int = wasm.native

  @WasmImport("foo", "setFooMarker")
  def setFooMarker(value: Int): Unit = wasm.native

  @WasmImport("foo", "throwRangeError")
  def throwRangeError(message: Int): Unit = wasm.native
}

object bar {
  @WasmImport("bar", "i8ArraySum")
  def i8ArraySum(xs: Array[Byte]): Int = wasm.native

  @WasmImport("bar", "i16ArraySum")
  def i16ArraySum(xs: Array[Short]): Int = wasm.native

  @WasmImport("bar", "i32ArraySum")
  def i32ArraySum(xs: Array[Int]): Int = wasm.native

  @WasmImport("bar", "i64ArraySum")
  def i64ArraySum(xs: Array[Long]): Long = wasm.native

  @WasmImport("bar", "f32ArraySum")
  def f32ArraySum(xs: Array[Float]): Float = wasm.native

  @WasmImport("bar", "f64ArraySum")
  def f64ArraySum(xs: Array[Double]): Double = wasm.native

  @WasmImport("bar", "getGlobalI8Array")
  def getGlobalI8Array(): Array[Byte] = wasm.native

  @WasmImport("bar", "getGlobalI16Array")
  def getGlobalI16Array(): Array[Short] = wasm.native

  @WasmImport("bar", "getGlobalI32Array")
  def getGlobalI32Array(): Array[Int] = wasm.native

  @WasmImport("bar", "getGlobalI64Array")
  def getGlobalI64Array(): Array[Long] = wasm.native

  @WasmImport("bar", "getGlobalF32Array")
  def getGlobalF32Array(): Array[Float] = wasm.native

  @WasmImport("bar", "getGlobalF64Array")
  def getGlobalF64Array(): Array[Double] = wasm.native
}

object WasmInterop {
  import utils._

  /** Explicitly return this value for test functions using assertEquals,
   *  to make sure that we get to the end and that no exception gets swallowed.
   */
  private final val OK = 777

  @WasmExport("i32Times5")
  def i32Times5(x: Int): Int = x * 5

  @WasmExport("i64Times5")
  def i64Times5(x: Long): Long = x * 5L

  @WasmExport("f32Times5dot5")
  def f32Times5dot5(x: Float): Float = x * 5.5f

  @WasmExport("f64Times5dot5")
  def f64Times5dot5(x: Double): Double = x * 5.5

  @WasmExport("combineNumericTypes") // different name on purpose
  def combineNumerics(a: Int, b: Long, c: Float, d: Double): Double =
    a + b * c + d

  private var unitResultMarker: Int = 0

  @WasmExport("unitResultMarker")
  def getUnitResultMarker(): Int = unitResultMarker

  @WasmExport("unitResult")
  def unitResult(value: Int): Unit =
    unitResultMarker = value

  @WasmExport("throwException")
  def throwException(tag: Int): Unit =
    throw new IllegalArgumentException(tag.toString())

  @WasmExport("numericImports")
  def numericImports(): Int = {
    assertEquals(44, foo.i32Times11(4))
    assertEquals(44L, foo.i64Times11(4L))
    assertEquals(48.875f, foo.f32Times11dot5(4.25f))
    assertEquals(48.875, foo.f64Times11dot5(4.25))

    OK
  }

  @WasmExport("unitResultImports")
  def unitResultImports(): Int = {
    assertEquals(0, foo.getFooMarker())
    foo.setFooMarker(432)
    assertEquals(432, foo.getFooMarker())

    OK
  }

  @nowarn("msg=Catching Throwable")
  @WasmExport("importsThatThrow")
  def importsThatThrow(): Int = {
    unitResultMarker = 3
    try {
      try {
        foo.throwRangeError(666)
        throw new AssertionError("throwRangeError did not throw")
      } catch {
        case _: Throwable =>
          // Dead code; we cannot catch JS exceptions
          unitResultMarker *= 5
          throw new AssertionError("should not get here")
      }
    } finally {
      // But we *can* run finally blocks
      unitResultMarker *= 7
    }
  }

  @WasmExport("i8ArraySum")
  def i8ArraySum(xs: Array[Byte]): Int = {
    val r = xs.sum.toInt
    xs(0) = 66 // mutates the copy; the original array is not affected
    r
  }

  @WasmExport("i16ArraySum")
  def i16ArraySum(xs: Array[Short]): Int = {
    val r = xs.sum.toInt
    xs(0) = 66
    r
  }

  @WasmExport("i32ArraySum")
  def i32ArraySum(xs: Array[Int]): Int = {
    val r = xs.sum
    xs(0) = 66
    r
  }

  @WasmExport("i64ArraySum")
  def i64ArraySum(xs: Array[Long]): Long = {
    val r = xs.sum
    xs(0) = 66L
    r
  }

  @WasmExport("f32ArraySum")
  def f32ArraySum(xs: Array[Float]): Float = {
    val r = xs.sum
    xs(0) = 66.0f
    r
  }

  @WasmExport("f64ArraySum")
  def f64ArraySum(xs: Array[Double]): Double = {
    val r = xs.sum
    xs(0) = 66.0
    r
  }

  val byteArray: Array[Byte] = Array(1, 2, 3, 4, 5)
  val shortArray: Array[Short] = Array(1, 2, 3, 4, 5)
  val intArray: Array[Int] = Array(1, 2, 3, 4, 5)
  val longArray: Array[Long] = Array(1, 2, 3, 4, 5)
  val floatArray: Array[Float] = Array(1, 2, 3, 4, 5)
  val doubleArray: Array[Double] = Array(1, 2, 3, 4, 5)

  @WasmExport("getByteArray")
  def getByteArray(): Array[Byte] = byteArray

  @WasmExport("getShortArray")
  def getShortArray(): Array[Short] = shortArray

  @WasmExport("getIntArray")
  def getIntArray(): Array[Int] = intArray

  @WasmExport("getLongArray")
  def getLongArray(): Array[Long] = longArray

  @WasmExport("getFloatArray")
  def getFloatArray(): Array[Float] = floatArray

  @WasmExport("getDoubleArray")
  def getDoubleArray(): Array[Double] = doubleArray

  @WasmExport("arrayParamImports")
  def arrayParamImports(): Int = {
    assertEquals(15, bar.i8ArraySum(byteArray))
    assertEquals(1.toByte, byteArray(0))

    assertEquals(15, bar.i16ArraySum(shortArray))
    assertEquals(1.toShort, shortArray(0))

    assertEquals(15, bar.i32ArraySum(intArray))
    assertEquals(1, intArray(0))

    assertEquals(15L, bar.i64ArraySum(longArray))
    assertEquals(1L, longArray(0))

    assertEquals(15.0f, bar.f32ArraySum(floatArray))
    assertEquals(1.0f, floatArray(0))

    assertEquals(15.0, bar.f64ArraySum(doubleArray))
    assertEquals(1.0, doubleArray(0))

    OK
  }

  @WasmExport("arrayResultImports")
  def arrayResultImports(): Int = {
    val i8Array = bar.getGlobalI8Array()
    assertEquals(2.toByte, i8Array(3))
    i8Array(0) = 66.toByte
    assertEquals(5.toByte, bar.getGlobalI8Array()(0))

    val i16Array = bar.getGlobalI16Array()
    assertEquals(2.toShort, i16Array(3))
    i16Array(0) = 66.toShort
    assertEquals(5.toShort, bar.getGlobalI16Array()(0))

    val i32Array = bar.getGlobalI32Array()
    assertEquals(2, i32Array(3))
    i32Array(0) = 66
    assertEquals(5, bar.getGlobalI32Array()(0))

    val i64Array = bar.getGlobalI64Array()
    assertEquals(2L, i64Array(3))
    i64Array(0) = 66L
    assertEquals(5L, bar.getGlobalI64Array()(0))

    val f32Array = bar.getGlobalF32Array()
    assertEquals(2.0f, f32Array(3))
    f32Array(0) = 66.0f
    assertEquals(5.0f, bar.getGlobalF32Array()(0))

    val f64Array = bar.getGlobalF64Array()
    assertEquals(2.0, f64Array(3))
    f64Array(0) = 66.0
    assertEquals(5.0, bar.getGlobalF64Array()(0))

    OK
  }
}
