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
