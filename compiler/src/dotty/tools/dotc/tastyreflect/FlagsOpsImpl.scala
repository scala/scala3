package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.core.Decorators._

import scala.tasty.reflect

trait FlagsOpsImpl extends scala.tasty.reflect.FlagsOps with CoreImpl {

  def FlagsDeco(flagSet: scala.tasty.reflect.FlagSet): FlagsAPI = new FlagsAPI {
    def is(that: reflect.FlagSet): Boolean = flagSet.asInstanceOf[FlagSet].flags is that.asInstanceOf[FlagSet].flags
    def |(that: reflect.FlagSet): reflect.FlagSet = new FlagSet(flagSet.asInstanceOf[FlagSet].flags | that.asInstanceOf[FlagSet].flags)
    def &(that: reflect.FlagSet): reflect.FlagSet = new FlagSet(flagSet.asInstanceOf[FlagSet].flags & that.asInstanceOf[FlagSet].flags)
  }

  object Flags extends FlagsModule {
    def Protected: FlagSet = new FlagSet(core.Flags.Protected)
    def Abstract: FlagSet = new FlagSet(core.Flags.Abstract)
    def Final: FlagSet = new FlagSet(core.Flags.Final)
    def Sealed: FlagSet = new FlagSet(core.Flags.Sealed)
    def Case: FlagSet = new FlagSet(core.Flags.Case)
    def Implicit: FlagSet = new FlagSet(core.Flags.Implicit)
    def Erased: FlagSet = new FlagSet(core.Flags.Erased)
    def Lazy: FlagSet = new FlagSet(core.Flags.Lazy)
    def Override: FlagSet = new FlagSet(core.Flags.Override)
    def Inline: FlagSet = new FlagSet(core.Flags.Inline)
    def Macro: FlagSet = new FlagSet(core.Flags.Macro)
    def Static: FlagSet = new FlagSet(core.Flags.JavaStatic)
    def Object: FlagSet = new FlagSet(core.Flags.Module)
    def Trait: FlagSet = new FlagSet(core.Flags.Trait)
    def Local: FlagSet = new FlagSet(core.Flags.Local)
    def Synthetic: FlagSet = new FlagSet(core.Flags.Synthetic)
    def Artifact: FlagSet = new FlagSet(core.Flags.Artifact)
    def Mutable: FlagSet = new FlagSet(core.Flags.Mutable)
    def FieldAccessor: FlagSet = new FlagSet(core.Flags.Accessor)
    def CaseAcessor: FlagSet = new FlagSet(core.Flags.CaseAccessor)
    def Covariant: FlagSet = new FlagSet(core.Flags.Covariant)
    def Contravariant: FlagSet = new FlagSet(core.Flags.Contravariant)
    def Scala2X: FlagSet = new FlagSet(core.Flags.Scala2x)
    def DefaultParameterized: FlagSet = new FlagSet(core.Flags.DefaultParameterized)
    def Stable: FlagSet = new FlagSet(core.Flags.Stable)
    def Param: FlagSet = new FlagSet(core.Flags.Param)
    def ParamAccessor: FlagSet = new FlagSet(core.Flags.ParamAccessor)
  }

}
