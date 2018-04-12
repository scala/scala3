package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.sourcePos
import dotty.tools.dotc.core.Flags._

import scala.tasty.modifiers

object Modifier {

  def apply(mod: untpd.Mod)(implicit ctx: Context): modifiers.Modifier = Impl(mod, ctx)

  private case class Impl(mod: untpd.Mod, ctx: Context) extends modifiers.Modifier {

    override def pos: scala.tasty.Position =  new Position(sourcePos(mod.pos)(ctx))

    override def isProtected: Boolean = mod.flags.is(Protected)
    override def isAbstract: Boolean = mod.flags.is(Abstract)
    override def isFinal: Boolean = mod.flags.is(Final)
    override def isSealed: Boolean = mod.flags.is(Sealed)
    override def isCase: Boolean = mod.flags.is(Case)
    override def isImplicit: Boolean = mod.flags.is(Implicit)
    override def isErased: Boolean = mod.flags.is(Erased)
    override def isLazy: Boolean = mod.flags.is(Lazy)
    override def isOverride: Boolean = mod.flags.is(Override)
    override def isInline: Boolean = mod.flags.is(Inline)
    override def isMacro: Boolean = mod.flags.is(Macro)
    override def isStatic: Boolean = mod.flags.is(JavaStatic)
    override def isObject: Boolean = mod.flags.is(Module)
    override def isTrait: Boolean = mod.flags.is(Trait)
    override def isLocal: Boolean = mod.flags.is(Local)
    override def isSynthetic: Boolean = mod.flags.is(Synthetic)
    override def isArtifact: Boolean = mod.flags.is(Artifact)
    override def isMutable: Boolean = mod.flags.is(Mutable)
    override def isLabel: Boolean = mod.flags.is(Label)
    override def isFieldAccessor: Boolean = mod.flags.is(Accessor)
    override def isCaseAcessor: Boolean = mod.flags.is(CaseAccessor)
    override def isCovariant: Boolean = mod.flags.is(Covariant)
    override def isContravariant: Boolean = mod.flags.is(Contravariant)
    override def isScala2X: Boolean = mod.flags.is(Scala2x)
    override def isDefaultParameterized: Boolean = mod.flags.is(DefaultParameterized)
    override def isStable: Boolean = mod.flags.is(Stable)

    override def toString: String = {
      if (isProtected) "Protected"
      else if (isAbstract) "Abstract"
      else if (isFinal) "Final"
      else if (isSealed) "Sealed"
      else if (isCase) "Case"
      else if (isImplicit) "Implicit"
      else if (isErased) "Erased"
      else if (isLazy) "Lazy"
      else if (isOverride) "Override"
      else if (isInline) "Inline"
      else if (isMacro) "Macro"
      else if (isStatic) "JavaStatic"
      else if (isObject) "Module"
      else if (isTrait) "Trait"
      else if (isLocal) "Local"
      else if (isSynthetic) "Synthetic"
      else if (isArtifact) "Artifact"
      else if (isMutable) "Mutable"
      else if (isLabel) "Label"
      else if (isFieldAccessor) "Accessor"
      else if (isCaseAcessor) "CaseAccessor"
      else if (isCovariant) "Covariant"
      else if (isContravariant) "Contravariant"
      else if (isScala2X) "Scala2x"
      else if (isDefaultParameterized) "DefaultParameterized"
      else if (isStable) "Stable"
      else "###"
    }
  }
}
