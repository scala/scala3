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

package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{AbstractOrTrait, Artifact, Bridge, Deferred, Enum, Final, JavaEnum, JavaVarargs, Mutable, Private, Synchronized, Trait}
import dotty.tools.dotc.core.Symbols.*

import scala.tools.asm

// TODO this object will be filled (a _lot_) when porting the optimizer
//      (one can argue we should reuse one of the other existing helper objects, but that's for a later cleanup)

object BCodeUtils {
  // The JVM enforces a max length of 65535 bytes per UTF-8 constant.
  // Java uses "Modified UTF-8", in which the null character specifically is two bytes,
  // and the rest of the BMP is as usual, see https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
  // Outside the BMP, characters are represented as surrogate pairs, i.e., 2+2 bytes, but `charAt` sees them as separate "characters".
  // This means if we see a surrogate pair in a string, we should count each half as 2 bytes, since the encoded UTF-8 character will be 4 bytes.
  // One consequence of this is that the maximum number of UTF-8 bytes for a single Java `char` (not codepoint!) is 3.
  private val MAX_BYTES_PER_UTF8_CONSTANT = 65535
  private val MAX_BYTES_PER_CHAR = 3

  /** Checks that the given name, or the concatenation of the given two names and descriptor if present, do not exceed the JVM's UTF-8 text size limits. */
  def checkConstantStringLength(name: String, other: String = ""): Boolean = {
    var byteCount = 0
    def check(str: String): Boolean =
      var i = 0
      while i < str.length do
        val c = str.charAt(i)
        byteCount += (
          if c == 0x00 then 2
          else if c <= 0x7F then 1
          else if c <= 0x7FF then 2
          else if Character.isHighSurrogate(c) || Character.isLowSurrogate(c) then 2
          else 3
        )
        if byteCount > MAX_BYTES_PER_UTF8_CONSTANT then
          return false
        i += 1
      true
    // For performance, since we expect few large strings, check if the string is obviously fine first.
    name.length + other.length <= MAX_BYTES_PER_UTF8_CONSTANT / MAX_BYTES_PER_CHAR || (check(name) && check(other))
  }

  /**
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final, strictfp (not used)
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   * for all:
   *  - deprecated
   *
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   */
  final def javaFlags(sym: Symbol)(using Context): Int = {
    import DottyBackendInterface.symExtensions

    // Classes are always emitted as public. This matches the behavior of Scala 2
    // and is necessary for object deserialization to work properly, otherwise
    // ModuleSerializationProxy may fail with an accessiblity error (see
    // tests/run/serialize.scala and https://github.com/typelevel/cats-effect/pull/2360).
    val privateFlag = !sym.isClass && (sym.is(Private) || (sym.isPrimaryConstructor && sym.owner.isTopLevelModuleClass))

    val finalFlag = sym.is(Final) && !toDenot(sym).isClassConstructor && !sym.isMutableVar && !sym.enclosingClass.is(Trait)

    import asm.Opcodes.*
    import GenBCodeOps.addFlagIf
    0 .addFlagIf(privateFlag, ACC_PRIVATE)
      .addFlagIf(!privateFlag, ACC_PUBLIC)
      .addFlagIf(sym.is(Deferred) || sym.isOneOf(AbstractOrTrait), ACC_ABSTRACT)
      .addFlagIf(sym.isInterface, ACC_INTERFACE)
      .addFlagIf(finalFlag
        // Primitives are "abstract final" to prohibit instantiation
        // without having to provide any implementations, but that is an
        // illegal combination of modifiers at the bytecode level so
        // suppress final if abstract if present.
        && !sym.isOneOf(AbstractOrTrait)
        // Bridges can be final, but final bridges confuse some frameworks
        && !sym.is(Bridge), ACC_FINAL)
      .addFlagIf(sym.isStaticMember, ACC_STATIC)
      .addFlagIf(sym.is(Bridge), ACC_BRIDGE | ACC_SYNTHETIC)
      .addFlagIf(sym.is(Artifact), ACC_SYNTHETIC)
      .addFlagIf(sym.isClass && !sym.isInterface, ACC_SUPER)
      .addFlagIf(sym.isAllOf(JavaEnum), ACC_ENUM)
      .addFlagIf(sym.is(JavaVarargs), ACC_VARARGS)
      .addFlagIf(sym.is(Synchronized), ACC_SYNCHRONIZED)
      .addFlagIf(sym.isDeprecated, ACC_DEPRECATED)
      .addFlagIf(sym.is(Enum), ACC_ENUM)
  }
}
