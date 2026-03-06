package scala.quoted.compiletime.internal

import dotty.tools.dotc
import scala.quoted.compiletime as pub

type Flags = FlagsImpl
final class FlagsImpl(val underlying: dotc.core.Flags.FlagSet) extends pub.Flags {

  override def is(that: pub.Flags): Boolean =
    val thatImpl = that.asInstanceOf[FlagsImpl]
    underlying.isAllOf(thatImpl.underlying)

  override def |(that: pub.Flags): pub.Flags =
    val thatImpl = that.asInstanceOf[FlagsImpl]
    new FlagsImpl(underlying | thatImpl.underlying)

  override def &(that: pub.Flags): pub.Flags =
    val thatImpl = that.asInstanceOf[FlagsImpl]
    new FlagsImpl(underlying & thatImpl.underlying)

  override def show: String = underlying.toString

  override def toString: String = underlying.toString
  override lazy val hashCode: Int = underlying.hashCode
  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: FlagsImpl => this.underlying == that.underlying
    case _               => false

}
object FlagsImpl {
  object Module extends pub.Flags.Module {
    def Abstract: pub.Flags = new FlagsImpl(dotc.core.Flags.Abstract)
    def AbsOverride: pub.Flags = new FlagsImpl(dotc.core.Flags.AbsOverride)
    def Artifact: pub.Flags = new FlagsImpl(dotc.core.Flags.Artifact)
    def Case: pub.Flags = new FlagsImpl(dotc.core.Flags.Case)
    def CaseAccessor: pub.Flags = new FlagsImpl(dotc.core.Flags.CaseAccessor)
    def Contravariant: pub.Flags = new FlagsImpl(dotc.core.Flags.Contravariant)
    def Covariant: pub.Flags = new FlagsImpl(dotc.core.Flags.Covariant)
    def Deferred: pub.Flags = new FlagsImpl(dotc.core.Flags.Deferred)
    def EmptyFlags: pub.Flags = new FlagsImpl(dotc.core.Flags.EmptyFlags)
    def Enum: pub.Flags = new FlagsImpl(dotc.core.Flags.Enum)
    def Erased: pub.Flags = new FlagsImpl(dotc.core.Flags.Erased)
    def Exported: pub.Flags = new FlagsImpl(dotc.core.Flags.Exported)
    def ExtensionMethod: pub.Flags = new FlagsImpl(dotc.core.Flags.ExtensionMethod)
    def FieldAccessor: pub.Flags = new FlagsImpl(dotc.core.Flags.Accessor)
    def Final: pub.Flags = new FlagsImpl(dotc.core.Flags.Final)
    def Given: pub.Flags = new FlagsImpl(dotc.core.Flags.Given)
    def HasDefault: pub.Flags = new FlagsImpl(dotc.core.Flags.HasDefault)
    def Implicit: pub.Flags = new FlagsImpl(dotc.core.Flags.Implicit)
    def Infix: pub.Flags = new FlagsImpl(dotc.core.Flags.Infix)
    def Inline: pub.Flags = new FlagsImpl(dotc.core.Flags.Inline)
    def Invisible: pub.Flags = new FlagsImpl(dotc.core.Flags.Invisible)
    def JavaDefined: pub.Flags = new FlagsImpl(dotc.core.Flags.JavaDefined)
    def JavaStatic: pub.Flags = new FlagsImpl(dotc.core.Flags.JavaStatic)
    def JavaAnnotation: pub.Flags = new FlagsImpl(dotc.core.Flags.JavaAnnotation)
    def Lazy: pub.Flags = new FlagsImpl(dotc.core.Flags.Lazy)
    def Local: pub.Flags = new FlagsImpl(dotc.core.Flags.Local)
    def Macro: pub.Flags = new FlagsImpl(dotc.core.Flags.Macro)
    def Method: pub.Flags = new FlagsImpl(dotc.core.Flags.Method)
    def Module: pub.Flags = new FlagsImpl(dotc.core.Flags.Module)
    def Mutable: pub.Flags = new FlagsImpl(dotc.core.Flags.Mutable)
    def NoInits: pub.Flags = new FlagsImpl(dotc.core.Flags.NoInits)
    def Opaque: pub.Flags = new FlagsImpl(dotc.core.Flags.Opaque)
    def Open: pub.Flags = new FlagsImpl(dotc.core.Flags.Open)
    def Override: pub.Flags = new FlagsImpl(dotc.core.Flags.Override)
    def Package: pub.Flags = new FlagsImpl(dotc.core.Flags.Package)
    def Param: pub.Flags = new FlagsImpl(dotc.core.Flags.Param)
    def ParamAccessor: pub.Flags = new FlagsImpl(dotc.core.Flags.ParamAccessor)
    def Private: pub.Flags = new FlagsImpl(dotc.core.Flags.Private)
    def PrivateLocal: pub.Flags = new FlagsImpl(dotc.core.Flags.Private | dotc.core.Flags.Local)
    def Protected: pub.Flags = new FlagsImpl(dotc.core.Flags.Protected)
    def Scala2x: pub.Flags = new FlagsImpl(dotc.core.Flags.Scala2x)
    def Sealed: pub.Flags = new FlagsImpl(dotc.core.Flags.Sealed)
    def StableRealizable: pub.Flags = new FlagsImpl(dotc.core.Flags.StableRealizable)
    def Static: pub.Flags = new FlagsImpl(dotc.core.Flags.JavaStatic)
    def Synthetic: pub.Flags = new FlagsImpl(dotc.core.Flags.Synthetic)
    def Trait: pub.Flags = new FlagsImpl(dotc.core.Flags.Trait)
    def Transparent: pub.Flags = new FlagsImpl(dotc.core.Flags.Transparent)
  }
}
