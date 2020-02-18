class Reflection(val internal: CompilerInterface) { self => // It works if the self is removed
  opaque type Flags = internal.Flags
  object Flags {
    def EmptyFlags: Flags = internal.Flags_EmptyFlags
  }
}

trait CompilerInterface {
  type Flags
  def Flags_EmptyFlags: Flags
}
