/*
 * Scala.js JS Envs (https://github.com/scala-js/scala-js-js-envs)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package build

import scala.collection.mutable

/** WebAssembly modules that expose functions to manipulate GC arrays.
 *
 *  For each numeric storage type `storageType`, provides the byte content of a
 *  WebAssembly module that manipulates GC arrays of `storageType`.
 *
 *  Each module exports 4 functions:
 *
 *  - `create: [i32] -> [(ref arrayType)]`
 *  - `length: [(ref arrayType)] -> [i32]`
 *  - `get: [(ref arrayType) i32] -> [valueType]`
 *  - `set: [(ref arrayType) i32 valueType] -> []`
 *
 *  where `arrayType` is an `(array (mut storageType))` and `valueType` is
 *  the value type corresponding to `storageType`.
 */
object WasmGCArrayAccessModules {

  val i8ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x78, valueTypeCode = 0x7f) // i8, i32

  val i16ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x77, valueTypeCode = 0x7f) // i16, i32

  val i32ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x7f, valueTypeCode = 0x7f) // i32, i32

  val i64ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x7e, valueTypeCode = 0x7e) // i64, i64

  val f32ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x7d, valueTypeCode = 0x7d) // f32, f32

  val f64ArrayModuleBytes: Array[Byte] =
    makeArrayModuleBytes(storageTypeCode = 0x7c, valueTypeCode = 0x7c) // f64, f64

  private def makeArrayModuleBytes(storageTypeCode: Byte,
      valueTypeCode: Byte): Array[Byte] = {

    val buf = new mutable.ArrayBuffer[Byte]()

    // magic header: null char + "asm"
    buf ++= Seq(0, 'a', 's', 'm')

    // version
    buf ++= Seq(1, 0, 0, 0)

    val i32Code = 0x7f.toByte

    val i32Type = Seq(i32Code)
    val storageType = Seq(storageTypeCode)
    val valueType = Seq(valueTypeCode)

    val NonNullableHeapTypePrefix = 0x64.toByte
    val arrayTypeID = 0.toByte
    val arrayType = Seq(NonNullableHeapTypePrefix, arrayTypeID)

    val arrayGet2ndByte: Byte =
      if (storageTypeCode == 0x77 || storageTypeCode == 0x78) 0x0c // array.get_s
      else 0x0b // array.get

    final case class FuncDesc(
      exportName: String,
      paramTypes: Seq[Seq[Byte]],
      resultTypes: Seq[Seq[Byte]],
      finalInstr: Seq[Byte]
    )

    val functions: Seq[FuncDesc] = Seq(
      FuncDesc(
        "create",
        Seq(i32Type),
        Seq(arrayType),
        Seq(0xfb.toByte, 0x07, arrayTypeID), // array.new_default $arrayTypeID
      ),
      FuncDesc(
        "length",
        Seq(arrayType),
        Seq(i32Type),
        Seq(0xfb.toByte, 0x0f), // array.length
      ),
      FuncDesc(
        "get",
        Seq(arrayType, i32Type),
        Seq(valueType),
        Seq(0xfb.toByte, arrayGet2ndByte, arrayTypeID), // array.get(_s) $arrayTypeID
      ),
      FuncDesc(
        "set",
        Seq(arrayType, i32Type, valueType),
        Seq(),
        Seq(0xfb.toByte, 0x0e, arrayTypeID), // array.set $arrayTypeID
      )
    )

    def writeSection(sectionID: Byte)(sectionContent: => Unit): Unit = {
      buf += sectionID
      byteLengthSubSection(buf)(sectionContent)
    }

    // Types section
    writeSection(sectionID = 0x01) {
      // number of types
      buf += (1 + functions.size).toByte

      // (type $theArray (array (mut $storageType))
      buf += 0x5e // array
      buf += storageTypeCode // $storageType
      buf += 1 // mut

      // function types
      for ((func, index) <- functions.zipWithIndex) {
        buf += 0x60 // func
        buf += func.paramTypes.size.toByte
        for (paramType <- func.paramTypes)
          buf ++= paramType
        buf += func.resultTypes.size.toByte
        for (resultType <- func.resultTypes)
          buf ++= resultType
      }
    }

    // Function section
    writeSection(sectionID = 0x03) {
      buf += functions.size.toByte

      for (index <- 0 until functions.size) {
        buf += (1 + index).toByte // the function's type ID
      }
    }

    // Export section
    writeSection(sectionID = 0x07) {
      buf += functions.size.toByte

      for ((func, index) <- functions.zipWithIndex) {
        // name
        buf += func.exportName.length().toByte
        buf ++= func.exportName.map(_.toByte)

        buf += 0x00 // func
        buf += index.toByte // func ID
      }
    }

    // Code section
    writeSection(sectionID = 0x0a) {
      buf += functions.size.toByte

      for (func <- functions) {
        byteLengthSubSection(buf) {
          buf += 0 // no locals

          // Load all the params
          for (index <- 0 until func.paramTypes.size) {
            buf += 0x20 // local.get
            buf += index.toByte
          }

          // Insert the final instruction bytes
          buf ++= func.finalInstr

          buf += 0x0b // end
        }
      }
    }

    buf.toArray
  }

  // !!! Adapted from webassembly.BinaryWriter
  private def byteLengthSubSection(buf: mutable.ArrayBuffer[Byte])(subSectionContent: => Unit): Unit = {
    // Reserve 4 bytes at the current offset to store the byteLength later
    val byteLengthOffset = buf.size
    for (_ <- 0 until 4) // write fake bytes for now
      buf += 0
    val startOffset = buf.size

    subSectionContent

    // Compute byteLength
    val endOffset = buf.size
    val byteLength = endOffset - startOffset

    /* Write the byteLength in the reserved slot. Note that we *always* use
     * 4 bytes to store the byteLength, even when less bytes are necessary in
     * the unsigned LEB encoding. The WebAssembly spec specifically calls out
     * this choice as valid. We leverage it to have predictable total offsets
     * when we write the code section, which is important to efficiently
     * generate source maps.
     */
    buf(byteLengthOffset) = ((byteLength & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 1) = (((byteLength >>> 7) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 2) = (((byteLength >>> 14) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 3) = ((byteLength >>> 21) & 0x7f).toByte
  }
}
