package scala.quoted.compiletime

trait Flags private[compiletime] () {

  /** Is the given flag set a subset of this flag sets. */
  def is(that: Flags): Boolean

  /** Union of the two flag sets. */
  def |(that: Flags): Flags

  /** Intersection of the two flag sets. */
  def &(that: Flags): Flags

  /** Shows the flags as a String. */
  def show: String

}
object Flags {

  def quoted(using quotes: Quotes): Flags.Module = quotes.reflectV2.Flags
  given moduleConversion: (quotes: Quotes) => Conversion[Flags.type, Flags.Module] = _ => quotes.reflectV2.Flags

  trait Module private[compiletime] () {
    def Abstract: Flags
    def AbsOverride: Flags
    def Artifact: Flags
    def Case: Flags
    def CaseAccessor: Flags
    def Contravariant: Flags
    def Covariant: Flags
    def Deferred: Flags
    def EmptyFlags: Flags
    def Enum: Flags
    def Erased: Flags
    def Exported: Flags
    def ExtensionMethod: Flags
    def FieldAccessor: Flags
    def Final: Flags
    def Given: Flags
    def HasDefault: Flags
    def Implicit: Flags
    def Infix: Flags
    def Inline: Flags
    def Invisible: Flags
    def JavaDefined: Flags
    def JavaStatic: Flags
    def JavaAnnotation: Flags
    def Lazy: Flags
    def Local: Flags
    def Macro: Flags
    def Method: Flags
    def Module: Flags
    def Mutable: Flags
    def NoInits: Flags
    def Opaque: Flags
    def Open: Flags
    def Override: Flags
    def Package: Flags
    def Param: Flags
    def ParamAccessor: Flags
    def Private: Flags
    def PrivateLocal: Flags
    def Protected: Flags
    def Scala2x: Flags
    def Sealed: Flags
    def StableRealizable: Flags
    @deprecated("Use JavaStatic instead", "3.3.0") def Static: Flags
    def Synthetic: Flags
    def Trait: Flags
    def Transparent: Flags
  }

}