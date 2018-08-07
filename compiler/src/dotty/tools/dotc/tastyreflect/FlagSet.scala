package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags._

class FlagSet(flags: Flags.FlagSet) extends scala.tasty.reflect.FlagSet {

  def isProtected: Boolean = flags.is(Protected)
  def isAbstract: Boolean = flags.is(Abstract)
  def isFinal: Boolean = flags.is(Final)
  def isSealed: Boolean = flags.is(Sealed)
  def isCase: Boolean = flags.is(Case)
  def isImplicit: Boolean = flags.is(Implicit)
  def isErased: Boolean = flags.is(Erased)
  def isLazy: Boolean = flags.is(Lazy)
  def isOverride: Boolean = flags.is(Override)
  def isTransparent: Boolean = flags.is(Transparent)
  def isMacro: Boolean = flags.is(Macro)
  def isStatic: Boolean = flags.is(JavaStatic)
  def isObject: Boolean = flags.is(Module)
  def isTrait: Boolean = flags.is(Trait)
  def isLocal: Boolean = flags.is(Local)
  def isSynthetic: Boolean = flags.is(Synthetic)
  def isArtifact: Boolean = flags.is(Artifact)
  def isMutable: Boolean = flags.is(Mutable)
  def isLabel: Boolean = flags.is(Label)
  def isFieldAccessor: Boolean = flags.is(Accessor)
  def isCaseAcessor: Boolean = flags.is(CaseAccessor)
  def isCovariant: Boolean = flags.is(Covariant)
  def isContravariant: Boolean = flags.is(Contravariant)
  def isScala2X: Boolean = flags.is(Scala2x)
  def isDefaultParameterized: Boolean = flags.is(DefaultParameterized)
  def isStable: Boolean = flags.is(Stable)
  def isParam: Boolean = flags.is(Param)
  def isParamAccessor: Boolean = flags.is(ParamAccessor)

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
    if (isTransparent) flags += "transparent"
    if (isMacro) flags += "macro"
    if (isStatic) flags += "javaStatic"
    if (isObject) flags += "object"
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
    if (isParam) flags += "param"
    if (isParamAccessor) flags += "paramAccessor"
    flags.result().mkString("<", ",", ">")
  }

}
