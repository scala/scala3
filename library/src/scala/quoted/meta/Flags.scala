package scala.quoted.meta

trait Flags private[meta] {

  /** Is the given flag set a subset of this flag sets */
  def is(that: Flags): Boolean

  /** Union of the two flag sets */
  def |(that: Flags): Flags

  /** Intersection of the two flag sets */
  def &(that: Flags): Flags

  /** Shows the flags as a String */
  def show: String

}
object Flags {

  def api(using meta: Meta): Meta.FlagsAPI = meta.internal.flags
  given Meta => Conversion[Flags.type, Meta.FlagsAPI] = _.api

}
