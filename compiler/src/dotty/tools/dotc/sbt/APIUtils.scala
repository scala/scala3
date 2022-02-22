package dotty.tools.dotc
package sbt

import core._
import Contexts._
import Flags._
import Symbols._
import NameOps._

import xsbti.api
import xsbti.api.SafeLazy.strict

/** Utilities to deal with xsbti.api.
 *
 *  Mostly comes from https://github.com/sbt/zinc/blob/c46643f3e68d7d4f270bf318e3f150f5a59c0aab/internal/zinc-apiinfo/src/main/scala/xsbt/api/APIUtil.scala
 */
object APIUtils {
  private object Constants {
    val PublicAccess = api.Public.create()
    val EmptyModifiers = new api.Modifiers(false, false, false, false, false, false, false, false)
    val EmptyStructure = api.Structure.of(strict(Array.empty), strict(Array.empty), strict(Array.empty))
    val EmptyType = api.EmptyType.of()
  }

  import Constants._

  /** Registers a dummy class for sbt's incremental compilation.
   *
   *  If a compiler phase creates a new named (module) class/trait after the phase
   *  `ExtractAPI`, it must register that class for sbt's incremental compilation
   *  on its own, lest crashes happen. In theory, the full API of the class needs
   *  to be constructed, but if the class is never accessed by Scala source code,
   *  a dummy empty class can be registered instead, using this method.
   */
  def registerDummyClass(classSym: ClassSymbol)(using Context): Unit = {
    if (ctx.sbtCallback != null) {
      val classLike = emptyClassLike(classSym)
      ctx.sbtCallback.api(ctx.compilationUnit.source.file.file, classLike)
    }
  }

  // See APIUtils.emptyClassLike
  private def emptyClassLike(classSym: ClassSymbol)(using Context): api.ClassLike = {
    val name = classSym.fullName.stripModuleClassSuffix.toString
    val definitionType =
      if (classSym.is(Trait)) api.DefinitionType.Trait
      else if (classSym.is(Module)) api.DefinitionType.Module
      else api.DefinitionType.ClassDef
    val topLevel = classSym.isTopLevelClass
    api.ClassLike.of(name, PublicAccess, EmptyModifiers, Array.empty, definitionType,
      strict(EmptyType), strict(EmptyStructure), Array.empty, Array.empty, topLevel, Array.empty)
  }
}
