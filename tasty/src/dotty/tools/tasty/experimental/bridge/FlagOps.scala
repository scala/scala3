package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait FlagOps extends Core with

  object Flags with
    val Protected: Flag = internal.Flags_Protected
    val ParamAccessor: Flag = internal.Flags_ParamAccessor
    val Private: Flag = internal.Flags_Private
    val Final: Flag = internal.Flags_Final
    val Case: Flag = internal.Flags_Case
    val Override: Flag = internal.Flags_Override
    val Inline: Flag = internal.Flags_Inline
    val InlineProxy: Flag = internal.Flags_InlineProxy
    val Macro: Flag = internal.Flags_Macro
    val JavaStatic: Flag = internal.Flags_JavaStatic
    val Module: Flag = internal.Flags_Module
    val Enum: Flag = internal.Flags_Enum
    val Local: Flag = internal.Flags_Local
    val Synthetic: Flag = internal.Flags_Synthetic
    val Artifact: Flag = internal.Flags_Artifact
    val Scala2x: Flag = internal.Flags_Scala2x
    val Implicit: Flag = internal.Flags_Implicit
    val Given: Flag = internal.Flags_Given
    val Erased: Flag = internal.Flags_Erased
    val Lazy: Flag = internal.Flags_Lazy
    val AbsOverride: Flag = internal.Flags_AbsOverride
    val Mutable: Flag = internal.Flags_Mutable
    val Accessor: Flag = internal.Flags_Accessor
    val CaseAccessor: Flag = internal.Flags_CaseAccessor
    val DefaultParameterized: Flag = internal.Flags_DefaultParameterized
    val StableRealizable: Flag = internal.Flags_StableRealizable
    val Extension: Flag = internal.Flags_Extension
    val Exported: Flag = internal.Flags_Exported
    val Label: Flag = internal.Flags_Label
    val Sealed: Flag = internal.Flags_Sealed
    val Abstract: Flag = internal.Flags_Abstract
    val Trait: Flag = internal.Flags_Trait
    val Covariant: Flag = internal.Flags_Covariant
    val Contravariant: Flag = internal.Flags_Contravariant
    val Opaque: Flag = internal.Flags_Opaque
    val Open: Flag = internal.Flags_Open
  end Flags

  given FlagSetOps: (flags: FlagSet)
    def is(flag: Flag): Boolean = internal.FlagSet_is(flags, flag)
    def is(flag: Flag, butNot: FlagSet): Boolean = internal.FlagSet_is(flags, flag, butNot)
    def &~(flag: Flag): FlagSet = internal.FlagSet_&~(flags, flag)
