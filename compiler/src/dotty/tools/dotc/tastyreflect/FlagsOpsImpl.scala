package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.core.Decorators._

import scala.tasty.reflect

trait FlagsOpsImpl extends scala.tasty.reflect.FlagsOps with CoreImpl {

  def FlagsDeco(flagSet: Flags): FlagsAPI = new FlagsAPI {
    def is(that: Flags): Boolean = flagSet is that
    def |(that: Flags): Flags = flagSet | that
    def &(that: Flags): Flags = flagSet & that
  }

  object Flags extends FlagsModule {
    def Private: Flags = core.Flags.Private
    def Protected: Flags = core.Flags.Protected
    def Abstract: Flags = core.Flags.Abstract
    def Final: Flags = core.Flags.Final
    def Sealed: Flags = core.Flags.Sealed
    def Case: Flags = core.Flags.Case
    def Implicit: Flags = core.Flags.Implicit
    def Implied = core.Flags.Implied
    def Erased: Flags = core.Flags.Erased
    def Lazy: Flags = core.Flags.Lazy
    def Override: Flags = core.Flags.Override
    def Inline: Flags = core.Flags.Inline
    def Macro: Flags = core.Flags.Macro
    def Static: Flags = core.Flags.JavaStatic
    def JavaDefined: Flags = core.Flags.JavaDefined
    def Object: Flags = core.Flags.Module
    def Trait: Flags = core.Flags.Trait
    def Local: Flags = core.Flags.Local
    def Synthetic: Flags = core.Flags.Synthetic
    def Artifact: Flags = core.Flags.Artifact
    def Mutable: Flags = core.Flags.Mutable
    def FieldAccessor: Flags = core.Flags.Accessor
    def CaseAcessor: Flags = core.Flags.CaseAccessor
    def Covariant: Flags = core.Flags.Covariant
    def Contravariant: Flags = core.Flags.Contravariant
    def Scala2X: Flags = core.Flags.Scala2x
    def DefaultParameterized: Flags = core.Flags.DefaultParameterized
    def StableRealizable: Flags = core.Flags.StableRealizable
    def Param: Flags = core.Flags.Param
    def ParamAccessor: Flags = core.Flags.ParamAccessor
    def Enum: Flags = core.Flags.Enum
    def ModuleClass: Flags = core.Flags.ModuleClass
    def PrivateLocal: Flags = core.Flags.PrivateLocal
    def Package: Flags = core.Flags.Package
    def ImplClass: Flags = core.Flags.ImplClass
  }

}
