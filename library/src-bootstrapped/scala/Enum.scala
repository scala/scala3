package scala

/** A Product that also describes a label and ordinal */
@deprecated("scala.Enum is no longer supported", "3.0.0-M1")
trait Enum extends Product, Serializable:

  /** A string uniquely identifying a case of an enum */
  def enumLabel: String

  /** A number uniquely identifying a case of an enum */
  def ordinal: Int
