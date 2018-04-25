package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Flags._

import scala.tasty.modifiers

object FlagsModifier {

  // TODO make sure all flags are tested

  def apply(sym: Symbol): modifiers.Flags = new Impl(sym)

  def unapplyFlags(arg: Impl)(implicit ctx: Context): Option[modifiers.Flags.Data] = {
    val sym = arg.sym
    Some(new modifiers.FlagSet {
      override def isProtected: Boolean = sym.is(Protected)
      override def isAbstract: Boolean = sym.is(Abstract)
      override def isFinal: Boolean = sym.is(Final)
      override def isSealed: Boolean = sym.is(Sealed)
      override def isCase: Boolean = sym.is(Case)
      override def isImplicit: Boolean = sym.is(Implicit)
      override def isErased: Boolean = sym.is(Erased)
      override def isLazy: Boolean = sym.is(Lazy)
      override def isOverride: Boolean = sym.is(Override)
      override def isInline: Boolean = sym.is(Inline)
      override def isMacro: Boolean = sym.is(Macro)
      override def isStatic: Boolean = sym.is(JavaStatic)
      override def isObject: Boolean = sym.is(Module)
      override def isTrait: Boolean = sym.is(Trait)
      override def isLocal: Boolean = sym.is(Local)
      override def isSynthetic: Boolean = sym.is(Synthetic)
      override def isArtifact: Boolean = sym.is(Artifact)
      override def isMutable: Boolean = sym.is(Mutable)
      override def isLabel: Boolean = sym.is(Label)
      override def isFieldAccessor: Boolean = sym.is(Accessor)
      override def isCaseAcessor: Boolean = sym.is(CaseAccessor)
      override def isCovariant: Boolean = sym.is(Covariant)
      override def isContravariant: Boolean = sym.is(Contravariant)
      override def isScala2X: Boolean = sym.is(Scala2x)
      override def isDefaultParameterized: Boolean = sym.is(DefaultParameterized)
      override def isStable: Boolean = sym.is(Stable)

      override def toString: String = {
        val flags = List.newBuilder[String]
        if (isProtected) flags += "protected "
        if (isAbstract) flags += "abstract"
        if (isFinal) flags += "final"
        if (isSealed) flags += "sealed"
        if (isCase) flags += "case"
        if (isImplicit) flags += "implicit"
        if (isErased) flags += "erased"
        if (isLazy) flags += "lazy"
        if (isOverride) flags += "override"
        if (isInline) flags += "inline"
        if (isMacro) flags += "macro"
        if (isStatic) flags += "javaStatic"
        if (isObject) flags += "module"
        if (isTrait) flags += "trait"
        if (isLocal) flags += "local"
        if (isSynthetic) flags += "synthetic"
        if (isArtifact) flags += "artifact"
        if (isMutable) flags += "mutable"
        if (isLabel) flags += "label"
        if (isFieldAccessor) flags += "accessor"
        if (isCaseAcessor) flags += "caseAccessor"
        if (isCovariant) flags += "covariant"
        if (isContravariant) flags += "contravariant"
        if (isScala2X) flags += "scala2x"
        if (isDefaultParameterized) flags += "defaultParameterized"
        if (isStable) flags += "stable"
        flags.result().mkString("<", ",", ">")
      }
    })
  }

  private[tasty] class Impl(val sym: Symbol) extends modifiers.Flags {
    override def toString: String = "Flags"
  }
}
